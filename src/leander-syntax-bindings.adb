with Leander.Core.Alts;
with Leander.Core.Bindings;
with Leander.Core.Patterns;
with Leander.Core.Types;
with Leander.Syntax.Expressions;

with WL.Graphs;
with WL.String_Maps;

package body Leander.Syntax.Bindings is

   package Alt_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Core.Alts.Reference, Leander.Core.Alts."=");

   type Implicit_Binding_Entry is
      record
         Name  : Leander.Names.Leander_Name;
         Alts  : Alt_Lists.List;
         Index : Positive;
      end record;

   package Implicit_Binding_Maps is
     new WL.String_Maps (Implicit_Binding_Entry);

   type Explicit_Binding_Entry is
      record
         Name  : Leander.Names.Leander_Name;
         Alts  : Alt_Lists.List;
         T     : Leander.Core.Types.Reference;
      end record;

   package Explicit_Binding_Maps is
     new WL.String_Maps (Explicit_Binding_Entry);

   function To_Binding_Group
     (Bindings : Name_Binding_Lists.List;
      Types    : Type_Binding_Lists.List)
      return Leander.Core.Binding_Groups.Reference;

   type Graph_Vertex is
      record
         Index : Positive;
         Name  : Leander.Names.Leander_Name;
      end record;

   function Get_Index (This : Graph_Vertex) return Positive
   is (This.Index);

   package Binding_Graphs is
     new WL.Graphs (Positive, Graph_Vertex, Float, 1.0,
                    Get_Index);

   package Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Core.Bindings.Reference, Core.Bindings."=");

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding
     (This      : in out Instance;
      Loc       : Source.Source_Location;
      Name      : String;
      Pats      : Patterns.Reference_Array;
      Expr      : not null access constant Expressions.Instance'Class)
   is
      use type Leander.Names.Leander_Name;
   begin
      if This.Bindings.Is_Empty
        or else This.Bindings.Last_Element.Name
          /= Leander.Names.To_Leander_Name (Name)
      then
         This.Bindings.Append
           (Name_Binding'
              (Leander.Names.To_Leander_Name (Name),
               []));
      end if;

      declare
         B : Name_Binding renames This.Bindings (This.Bindings.Last);
         R : constant Binding_Record := Binding_Record'
           (Pat_Count => Pats'Length,
            Pats      => Pats,
            Expr      => Expression_Reference (Expr));
      begin
         B.Equations.Append (R);
      end;
   end Add_Binding;

   --------------
   -- Add_Type --
   --------------

   procedure Add_Type
     (This      : in out Instance;
      Loc       : Source.Source_Location;
      Name      : String;
      Type_Expr : Leander.Syntax.Types.Reference)
   is
   begin
      This.Types.Append
        (Type_Binding'
           (Leander.Names.To_Leander_Name (Name), Type_Expr));
   end Add_Type;

   -----------
   -- Empty --
   -----------

   function Empty return Reference is
   begin
      return new Instance;
   end Empty;

   ----------------------
   -- To_Binding_Group --
   ----------------------

   function To_Binding_Group
     (Bindings : Name_Binding_Lists.List;
      Types    : Type_Binding_Lists.List)
      return Leander.Core.Binding_Groups.Reference
   is
      Implicit : Implicit_Binding_Maps.Map;
      Explicit : Explicit_Binding_Maps.Map;
      Next     : Natural := 0;
      Graph    : Binding_Graphs.Graph;

      function References
        (Binding : Implicit_Binding_Entry;
         Name    : Leander.Names.Leander_Name)
         return Boolean;

      function To_Alts (Equations : Binding_Record_Lists.List)
                        return Alt_Lists.List;

      ----------------
      -- References --
      ----------------

      function References
        (Binding : Implicit_Binding_Entry;
         Name    : Leander.Names.Leander_Name)
         return Boolean
      is
      begin
         for Equation of Binding.Alts loop
            if Equation.Has_Reference (Core.Varid (Name)) then
               return True;
            end if;
         end loop;
         return False;
      end References;

      -------------
      -- To_Alts --
      -------------

      function To_Alts (Equations : Binding_Record_Lists.List)
                        return Alt_Lists.List
      is
      begin
         return List : Alt_Lists.List do
            for Equation of Equations loop
               declare
                  Pats : constant Leander.Core.Patterns.Reference_Array :=
                           [for Pat of Equation.Pats => Pat.To_Core];
               begin
                  List.Append
                    (Leander.Core.Alts.Alt
                       (Pats, Equation.Expr.To_Core));
               end;
            end loop;
         end return;
      end To_Alts;

   begin
      for Binding of Bindings loop
         Implicit.Insert
           (Leander.Names.To_String (Binding.Name),
            Implicit_Binding_Entry'
              (Binding.Name, To_Alts (Binding.Equations), 1));
      end loop;
      for Type_Binding of Types loop
         declare
            Key : constant String :=
                    Leander.Names.To_String (Type_Binding.Name);
         begin
            if not Implicit.Contains (Key) then
               raise Constraint_Error with
               Source.Show (Type_Binding.Type_Expr.Location)
                 & ": no value for type binding";
            end if;
            Explicit.Insert
              (Key,
               Explicit_Binding_Entry'
                 (Name => Type_Binding.Name,
                  Alts => Implicit.Element (Key).Alts,
                  T    => Type_Binding.Type_Expr.To_Core));
            Implicit.Delete (Key);
         end;
      end loop;

      for Binding of Implicit loop
         Next := Next + 1;
         Binding.Index := Next;
         Graph.Append (Graph_Vertex'(Next, Binding.Name));
      end loop;

      for Binding of Implicit loop
         for Other of Implicit loop
            if Binding.Index /= Other.Index
              and then not Graph.Connected (Other.Index, Binding.Index)
              and then (References (Binding, Other.Name)
                        or else References (Other, Binding.Name))
            then
               Graph.Connect (Other.Index, Binding.Index);
               Graph.Connect (Binding.Index, Other.Index);
            end if;
         end loop;
      end loop;

      declare
         Subgraphs : Binding_Graphs.Sub_Graph_Collection;
      begin
         Graph.Get_Connected_Components (Subgraphs);

         declare
            --  Count : constant Natural :=
            --            Binding_Graphs.Sub_Graph_Count (Subgraphs);
            Builder : Core.Binding_Groups.Instance_Builder;

            function To_Core_Bindings
              (G : Binding_Graphs.Sub_Graph)
               return Core.Bindings.Reference_Array;

            ----------------------
            -- To_Core_Bindings --
            ----------------------

            function To_Core_Bindings
              (G : Binding_Graphs.Sub_Graph)
               return Core.Bindings.Reference_Array
            is
               List : Binding_Lists.List;

               procedure Add (Vertex : Graph_Vertex);

               ---------
               -- Add --
               ---------

               procedure Add (Vertex : Graph_Vertex) is
                  Binding : constant Implicit_Binding_Entry :=
                              Implicit.Element
                                (Leander.Names.To_String (Vertex.Name));

                  function Get_Alts
                    return Leander.Core.Alts.Reference_Array;

                  --------------
                  -- Get_Alts --
                  --------------

                  function Get_Alts
                    return Leander.Core.Alts.Reference_Array
                  is
                  begin
                     return [for Alt of Binding.Alts => Alt];
                  end Get_Alts;

               begin
                  List.Append
                    (Core.Bindings.Implicit_Binding
                       (Core.Varid (Binding.Name), Get_Alts));
               end Add;

            begin
               Binding_Graphs.Iterate (G, Add'Access);
               return [for Ref of List => Ref];
            end To_Core_Bindings;

         begin
            for I in 1 .. Binding_Graphs.Sub_Graph_Count (Subgraphs)  loop
               Builder.Add_Implicit_Bindings
                 (To_Core_Bindings
                    (Binding_Graphs.Get_Sub_Graph (Subgraphs, I)));
            end loop;
            return Builder.Get_Binding_Group;
         end;
      end;

   end To_Binding_Group;

   -------------
   -- To_Core --
   -------------

   function To_Core
     (This : Instance)
      return Leander.Core.Binding_Groups.Reference
   is
   begin
      return To_Binding_Group (This.Bindings, This.Types);
   end To_Core;

   --     Container : constant Leander.Core.Bindings.Container_Reference :=
   --                   Leander.Core.Bindings.Container
   --                     ([for B of This.Bindings =>
   --                         Core.Bindings.Implicit_Binding
   --                           (B.Name,
   --                            B.Equations.First_Element.Expr.To_Core)]);
   --  begin
   --     return Leander.Core.Binding_Groups.Binding_Group
   --       ([Container]);
   --  end To_Core;

end Leander.Syntax.Bindings;
