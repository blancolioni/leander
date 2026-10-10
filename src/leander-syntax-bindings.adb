with Leander.Core;
with Leander.Core.Alts;
with Leander.Core.Bindings.Dependencies;
with Leander.Core.Qualified_Types;
with Leander.Core.Schemes;
with Leander.Syntax.Bindings.Guards;
with Leander.Syntax.Bindings.Transform;
with Leander.Syntax.Expressions;

with WL.String_Maps;

package body Leander.Syntax.Bindings is

   type Nullable_Type_Reference is
     access constant Leander.Core.Qualified_Types.Instance'Class;

   type Binding_Entry (Alt_Count : Natural) is
      record
         Name  : Leander.Names.Leander_Name;
         Alts  : Leander.Core.Alts.Reference_Array (1 .. Alt_Count);
         T     : Nullable_Type_Reference;
      end record;

   package Binding_Maps is
     new WL.String_Maps (Binding_Entry);

   function To_Binding_Group
     (Bindings      : Name_Binding_Lists.List;
      Types         : Type_Binding_Lists.List;
      Context       : Core.Declaration_Context;
      Predicates    : Core.Predicates.Predicate_Array;
      Monomorphic   : Boolean)
      return Leander.Core.Binding_Groups.Reference;

   function Allocate
     (This : Instance)
      return Reference
   is (Reference (Syntax.Allocate (This)));

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding
     (This        : in out Instance;
      Loc         : Source.Source_Location;
      Bound       : Binding_LHS;
      Expr        : not null access constant Expressions.Instance'Class;
      Fallthrough : String := "")
   is
   begin
      Add_Binding (This, Loc, Bound.Name, Bound.Pats, Expr, Fallthrough);
   end Add_Binding;

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding
     (This        : in out Instance;
      Loc         : Source.Source_Location;
      Name        : String;
      Pats        : Patterns.Reference_Array;
      Expr        : not null access constant Expressions.Instance'Class;
      Fallthrough : String := "")
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
           (Pat_Count   => Pats'Length,
            Pats        => Pats,
            Expr        => Expression_Reference (Expr),
            Fallthrough =>
              Ada.Strings.Unbounded.To_Unbounded_String (Fallthrough));
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
      Type_Expr : Leander.Syntax.Qualified_Types.Reference)
   is
      use type Leander.Core.Declaration_Context;
      use type Leander.Core.Predicates.Predicate_Array;
      Bound_Type : Leander.Syntax.Qualified_Types.Reference := Type_Expr;
   begin
      if This.Context = Leander.Core.Class_Context then
         declare
            use Leander.Core.Predicates;
            Ps : constant Predicate_Array :=
                   [for P of This.Predicates => P];
            Qs : constant Predicate_Array :=
                   Bound_Type.Predicates;
         begin
            Bound_Type :=
              Leander.Syntax.Qualified_Types.Qualified_Type
                (Ps & Qs, Bound_Type.Get_Type);
         end;
      end if;

      This.Types.Append
        (Type_Binding'
           (Leander.Names.To_Leander_Name (Name), Bound_Type));
   end Add_Type;

   -----------
   -- Empty --
   -----------

   function Empty
     (Context     : Core.Declaration_Context := Core.Binding_Context;
      Predicates  : Leander.Core.Predicates.Predicate_Array := [];
      Monomorphic : Boolean := False)
      return Reference
   is
   begin
      return Allocate (Instance'
        (Context     => Context,
         Predicates  => [for P of Predicates => P],
         Monomorphic => Monomorphic,
         others      => <>));
   end Empty;

   ----------------------
   -- To_Binding_Group --
   ----------------------

   function To_Binding_Group
     (Bindings      : Name_Binding_Lists.List;
      Types         : Type_Binding_Lists.List;
      Context       : Core.Declaration_Context;
      Predicates    : Core.Predicates.Predicate_Array;
      Monomorphic   : Boolean)
      return Leander.Core.Binding_Groups.Reference
   is
      pragma Unreferenced (Predicates);
      use Leander.Core;
      Implicit : Binding_Maps.Map;
      Explicit : Binding_Maps.Map;

   begin
      for Binding of Bindings loop
         declare
            Equations : Binding_Record_Lists.List := Binding.Equations;
         begin
            --  Guards first: a failed guard has to continue at the next
            --  equation, and once Transform.To_Alts has filed the
            --  equations by constructor there is no longer a next one to
            --  continue at.  See Leander.Syntax.Bindings.Guards.
            Guards.Lower (Binding.Name, Equations);

            declare
               Alts : constant Leander.Core.Alts.Reference_Array :=
                        Transform.To_Alts (Equations);
            begin
               Implicit.Insert
                 (Leander.Names.To_String (Binding.Name),
                  Binding_Entry'
                    (Alt_Count => Alts'Length,
                     Name      => Binding.Name,
                     Alts      => Alts,
                     T         => null));
            end;
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
               if Context = Class_Context then
                  Explicit.Insert
                    (Key,
                     Binding_Entry'
                       (Alt_Count => 0,
                        Name      => Type_Binding.Name,
                        Alts      => [],
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

      declare
         Bs : constant Core.Bindings.Reference_Array :=
                [for Binding of Implicit =>
                   Core.Bindings.Implicit_Binding
                     (Core.Varid (Binding.Name),
                      [for Alt of Binding.Alts => Alt],
                      Monomorphic => Monomorphic)];

         function Depends (From, To : Positive) return Boolean;

         -------------
         -- Depends --
         -------------

         function Depends (From, To : Positive) return Boolean is
         begin
            --  An instance method's body names the class's method, not the
            --  instance's own binding of it, so a reference only one way
            --  is no dependency at all.
            return Bs (From).Has_Reference (Bs (To).Name)
              and then (Context /= Instance_Context
                        or else Bs (To).Has_Reference (Bs (From).Name));
         end Depends;

         Component : constant Core.Bindings.Dependencies.Component_Array :=
                       Core.Bindings.Dependencies.Components
                         (Bs, Depends'Access);
         Builder   : Core.Binding_Groups.Instance_Builder;

         function To_Scheme
           (T : Nullable_Type_Reference)
            return Leander.Core.Schemes.Reference;

         ---------------
         -- To_Scheme --
         ---------------

         function To_Scheme
           (T : Nullable_Type_Reference)
            return Leander.Core.Schemes.Reference
         is
         begin
            return Core.Schemes.Quantify
              (T.Get_Tyvars, T);
         end To_Scheme;

      begin
         Builder.Add_Explicit_Bindings
           ([for Binding of Explicit =>
                 Core.Bindings.Explicit_Binding
               (Core.Varid (Binding.Name),
                [for Alt of Binding.Alts => Alt],
                To_Scheme (Binding.T))
            ]);

         --  One implicit group per dependency component, dependencies
         --  first: a group is generalised before anything that uses it is
         --  inferred, so each use instantiates it afresh.
         for C in 1 .. Core.Bindings.Dependencies.Component_Count (Component)
         loop
            declare
               Group : Core.Bindings.Reference_Array (1 .. Bs'Length);
               Count : Natural := 0;
            begin
               for I in Bs'Range loop
                  if Component (I) = C then
                     Count := Count + 1;
                     Group (Count) := Bs (I);
                  end if;
               end loop;
               Builder.Add_Implicit_Bindings (Group (1 .. Count));
            end;
         end loop;
         return Builder.Get_Binding_Group;
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
      return To_Binding_Group
        (This.Bindings, This.Types, This.Context,
         (if This.Predicates.Is_Empty
          then []
          else [for P of This.Predicates => P]),
         This.Monomorphic);
   end To_Core;

end Leander.Syntax.Bindings;
