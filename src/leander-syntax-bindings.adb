with Leander.Core.Alts;
with Leander.Core.Bindings;
with Leander.Core.Schemes;
with Leander.Core.Types;
with Leander.Syntax.Bindings.Transform;
with Leander.Syntax.Expressions;

with WL.Graphs;
with WL.String_Maps;

package body Leander.Syntax.Bindings is

   type Nullable_Type_Reference is
     access constant Leander.Core.Types.Instance'Class;

   type Binding_Entry (Alt_Count : Natural) is
      record
         Name  : Leander.Names.Leander_Name;
         Alts  : Leander.Core.Alts.Reference_Array (1 .. Alt_Count);
         Index : Positive;
         T     : Nullable_Type_Reference;
      end record;

   package Binding_Maps is
     new WL.String_Maps (Binding_Entry);

   function To_Binding_Group
     (Bindings      : Name_Binding_Lists.List;
      Types         : Type_Binding_Lists.List;
      Class_Context : Boolean)
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
     (Bindings      : Name_Binding_Lists.List;
      Types         : Type_Binding_Lists.List;
      Class_Context : Boolean)
      return Leander.Core.Binding_Groups.Reference
   is
      Implicit : Binding_Maps.Map;
      Explicit : Binding_Maps.Map;
      Next     : Natural := 0;
      Graph    : Binding_Graphs.Graph;

      function References
        (Binding : Binding_Entry;
         Name    : Leander.Names.Leander_Name)
         return Boolean;

      --  function To_Alts (Equations : Binding_Record_Lists.List)
      --                    return Alt_Lists.List;

      ----------------
      -- References --
      ----------------

      function References
        (Binding : Binding_Entry;
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

      --  function To_Alts (Equations : Binding_Record_Lists.List)
      --                    return Alt_Lists.List
      --  is
      --  begin
      --     return List : Alt_Lists.List do
      --        for Equation of Equations loop
      --           declare
      --              Pats : constant Core.Patterns.Reference_Array :=
      --                       [for Pat of Equation.Pats =>
      --                                 Pat.To_Core];
      --           begin
      --              if Pats'Length = 0 then
      --                 List.Append
      --                   (Leander.Core.Alts.Alt
      --                      (Equation.Expr.To_Core));
      --              elsif Pats'Length = 1 then
      --                 List.Append
      --                   (Leander.Core.Alts.Alt
      --                      (Pats (Pats'First), Equation.Expr.To_Core));
      --              else
      --                 raise Constraint_Error with
      --                   "only single patterns supported";
      --              end if;
      --           end;
      --        end loop;
      --     end return;
      --  end To_Alts;

   begin
      for Binding of Bindings loop
         declare
            Alts : constant Leander.Core.Alts.Reference_Array :=
                     Transform.To_Alts (Binding.Equations);
         begin
            Implicit.Insert
              (Leander.Names.To_String (Binding.Name),
               Binding_Entry'
                 (Alt_Count => Alts'Length,
                  Name      => Binding.Name,
                  Alts      => Alts,
                  Index     => 1,
                  T         => null));
         exception
            when others =>
               raise Program_Error with
                 "problem in binding for "
                 & Leander.Names.To_String (Binding.Name);
         end;
      end loop;
      for Type_Binding of Types loop
         declare
            Key : constant String :=
                    Leander.Names.To_String (Type_Binding.Name);
         begin
            if not Implicit.Contains (Key) then
               if Class_Context then
                  Explicit.Insert
                    (Key,
                     Binding_Entry'
                       (Alt_Count => 0,
                        Name      => Type_Binding.Name,
                        Alts      => [],
                        Index     => 1,
                        T         => Nullable_Type_Reference
                          (Type_Binding.Type_Expr.To_Core)));
               else
                  raise Constraint_Error with
                  Source.Show (Type_Binding.Type_Expr.Location)
                    & ": no value for type binding";
               end if;
            else
               Explicit.Insert
                 (Key,
                  Binding_Entry'
                    (Implicit.Element (Key) with delta
                         T    => Nullable_Type_Reference
                       (Type_Binding.Type_Expr.To_Core)));
               Implicit.Delete (Key);
            end if;
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

            function To_Scheme
              (T : Nullable_Type_Reference)
               return Leander.Core.Schemes.Reference;

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
                  Binding : constant Binding_Entry :=
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

            ---------------
            -- To_Scheme --
            ---------------

            function To_Scheme
              (T : Nullable_Type_Reference)
               return Leander.Core.Schemes.Reference
            is
            begin
               return Core.Schemes.Quantify
                 (T.Get_Tyvars,
                  Core.Types.Reference (T));
            end To_Scheme;

         begin
            Builder.Add_Explicit_Bindings
              ([for Binding of Explicit =>
                    Core.Bindings.Explicit_Binding
                  (Core.Varid (Binding.Name),
                   [for Alt of Binding.Alts => Alt],
                   To_Scheme (Binding.T))
               ]);

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
     (This          : Instance;
      Class_Context : Boolean := False)
      return Leander.Core.Binding_Groups.Reference
   is
   begin
      return To_Binding_Group (This.Bindings, This.Types, Class_Context);
   end To_Core;

end Leander.Syntax.Bindings;
