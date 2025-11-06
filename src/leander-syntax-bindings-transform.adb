with Ada.Containers.Indefinite_Doubly_Linked_Lists;

with Leander.Core.Bindings;
with Leander.Core.Expressions;
with Leander.Core.Patterns;
with Leander.Syntax.Expressions;

package body Leander.Syntax.Bindings.Transform is

   type Variable_Pattern (Pat_Count : Natural) is
      record
         Location : Leander.Source.Source_Location;
         Patvar   : Leander.Core.Patterns.Reference;
         Equation : Binding_Record (Pat_Count);
      end record;

   type Literal_Pattern (Pat_Count : Natural) is
      record
         Location : Leander.Source.Source_Location;
         Literal  : Leander.Core.Patterns.Reference;
         Equation : Binding_Record (Pat_Count);
      end record;

   function To_Core_Alt
     (Pat : Literal_Pattern)
      return Leander.Core.Alts.Reference;

   type Constructor_Pattern (Arg_Count : Natural; Pat_Count : Natural) is
      record
         Location : Leander.Source.Source_Location;
         Con      : Leander.Core.Conid;
         Args     : Leander.Core.Patterns.Conargs (1 .. Arg_Count);
         Equation : Binding_Record (Pat_Count);
      end record;

   function To_Core_Alt
     (Pat : Constructor_Pattern)
      return Leander.Core.Alts.Reference;

   package Variable_Pattern_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Variable_Pattern);

   function To_Core_Alt
     (Loc  : Leander.Source.Source_Location;
      Vars : Variable_Pattern_Lists.List)
      return Leander.Core.Alts.Reference;

   package Literal_Pattern_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Literal_Pattern);

   package Constructor_Pattern_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Constructor_Pattern);

   type Partial_Binding_Record is
      record
         Literals : Literal_Pattern_Lists.List;
         Cons     : Constructor_Pattern_Lists.List;
         Vars     : Variable_Pattern_Lists.List;
      end record;

   function To_Multipattern_Alts
     (Equations : Binding_Record_Lists.List)
      return Leander.Core.Alts.Reference_Array;

   function To_Core_Expression
     (Loc     : Leander.Source.Source_Location;
      Partial : Binding_Record;
      Vars    : Leander.Names.Name_Array)
      return Leander.Core.Expressions.Reference;

   -------------
   -- To_Alts --
   -------------

   function To_Alts
     (Equations : Binding_Record_Lists.List)
      return Leander.Core.Alts.Reference_Array
   is
      Count : constant Natural := Natural (Equations.Length);
      Last  : Natural := 0;
   begin
      if Equations.First_Element.Pat_Count = 0 then
         return [Leander.Core.Alts.Alt
                 (Equations.First_Element.Expr.To_Core)];
      elsif Equations.First_Element.Pat_Count = 1 then
         return Alts : Leander.Core.Alts.Reference_Array (1 .. Count) do
            for Equation of Equations loop
               Last := Last + 1;
               Alts (Last) :=
                 Leander.Core.Alts.Alt
                   (Equation.Pats (1).To_Core, Equation.Expr.To_Core);
            end loop;
         end return;
      else
         return Alts : constant Leander.Core.Alts.Reference_ARray :=
           To_Multipattern_Alts (Equations);
      end if;
   end To_Alts;

   -----------------
   -- To_Core_Alt --
   -----------------

   function To_Core_Alt
     (Pat : Literal_Pattern)
      return Leander.Core.Alts.Reference
   is
   begin
      return Leander.Core.Alts.Alt
        (Pat.Literal,
         To_Core_Expression (Pat.Location, Pat.Equation, []));
   end To_Core_Alt;

   -----------------
   -- To_Core_Alt --
   -----------------

   function To_Core_Alt
     (Pat : Constructor_Pattern)
      return Leander.Core.Alts.Reference
   is
   begin
      return Leander.Core.Alts.Alt
        (Leander.Core.Patterns.Constructor
           (Pat.Con, Pat.Args),
         To_Core_Expression (Pat.Location, Pat.Equation, []));
   end To_Core_Alt;

   -----------------
   -- To_Core_Alt --
   -----------------

   function To_Core_Alt
     (Loc  : Leander.Source.Source_Location;
      Vars : Variable_Pattern_Lists.List)
      return Leander.Core.Alts.Reference
   is
      use type Core.Varid;
      Have_Variable : Boolean := False;
      V             : Core.Varid :=
                        Vars.First_Element.Patvar.Variable;
      Equations     : Binding_Record_Lists.List;
   begin
      for Varpat of Vars loop
         if Varpat.Patvar.Is_Wildcard then
            Equations.Append
              (Binding_Record'
                 (Pat_Count => Varpat.Pat_Count,
                  Pats      => Varpat.Equation.Pats,
                  Expr      => Varpat.Equation.Expr));
         else
            if not Have_Variable then
               V := Varpat.Patvar.Variable;
               Have_Variable := True;
            end if;
            if V = Varpat.Patvar.Variable then
               Equations.Append
                 (Binding_Record'
                    (Pat_Count => Varpat.Pat_Count,
                     Pats      => Varpat.Equation.Pats,
                     Expr      => Varpat.Equation.Expr));
            else
               Equations.Append
                 (Binding_Record'
                    (Pat_Count => Varpat.Pat_Count,
                     Pats      => Varpat.Equation.Pats,
                     Expr      =>
                       Expression_Reference
                         (Syntax.Expressions.Application
                              (Varpat.Equation.Expr.Location,
                               Syntax.Expressions.Lambda
                                 (Varpat.Equation.Expr.Location,
                                  Syntax.Patterns.Variable
                                    (Varpat.Equation.Expr.Location,
                                     Core.To_String (Varpat.Patvar.Variable)),
                                  Varpat.Equation.Expr),
                               Syntax.Expressions.Variable
                                 (Varpat.Equation.Expr.Location,
                                  Core.To_String (V))))));
            end if;
         end if;
      end loop;

      declare
         Alts    : constant Leander.Core.Alts.Reference_Array :=
                     To_Alts (Equations);
         Builder : Core.Binding_Groups.Instance_Builder;
         F       : constant Core.Varid := Core.Varid (Leander.Names.New_Name);
         X       : constant Core.Varid := Core.Varid (Leander.Names.New_Name);
      begin
         if not Have_Variable then
            V := Core.Varid (Leander.Names.New_Name);
         end if;
         Builder.Add_Implicit_Bindings
           ([Core.Bindings.Implicit_Binding
            (F, Alts)]);
         return E : constant Core.Alts.Reference :=
           Core.Alts.Alt
             (Core.Patterns.Variable (V),
              Core.Expressions.Lambda
                (Loc, X,
                 Core.Expressions.Let
                   (Loc      => Loc,
                    Bindings => Builder.Get_Binding_Group,
                    Expr     =>
                      Core.Expressions.Application
                        (Loc,
                         Core.Expressions.Variable
                           (Loc, F),
                         Core.Expressions.Variable
                           (Loc, X)))));
      end;
   end To_Core_Alt;

   ------------------------
   -- To_Core_Expression --
   ------------------------

   function To_Core_Expression
     (Loc     : Leander.Source.Source_Location;
      Partial : Binding_Record;
      Vars    : Leander.Names.Name_Array)
      return Leander.Core.Expressions.Reference
   is
      pragma Unreferenced (Vars);
      Alts    : constant Leander.Core.Alts.Reference_Array :=
                  To_Alts ([Partial]);
      Builder : Core.Binding_Groups.Instance_Builder;
      F       : constant Core.Varid := Core.Varid (Leander.Names.New_Name);
   begin
      Builder.Add_Implicit_Bindings
        ([Core.Bindings.Implicit_Binding
         (F, Alts)]);

      declare
         E : Core.Expressions.Reference :=
               Core.Expressions.Variable (Loc, F);
      begin
         E :=
           Core.Expressions.Let
             (Loc      => Loc,
              Bindings => Builder.Get_Binding_Group,
              Expr     => E);
         return E;
      end;
   end To_Core_Expression;

   --------------------------
   -- To_Multipattern_Alts --
   --------------------------

   function To_Multipattern_Alts
     (Equations : Binding_Record_Lists.List)
      return Leander.Core.Alts.Reference_Array
   is
      Partial : Partial_Binding_Record;
   begin
      for Equation of Equations loop
         declare
            Pat  : constant Syntax.Patterns.Reference :=
                     Equation.Pats (1);
            Rest : constant Binding_Record :=
                     Binding_Record'
                       (Equation.Pat_Count - 1,
                        Equation.Pats (2 .. Equation.Pat_Count),
                        Equation.Expr);
         begin
            if Pat.Is_Variable then
               Partial.Vars.Append
                 (Variable_Pattern'
                    (Rest.Pat_Count, Pat.Location, Pat.To_Core, Rest));
            elsif Pat.Is_Constructor then
               declare
                  Args : constant Syntax.Patterns.Reference_Array :=
                           Pat.Constructor_Args;
               begin
                  Partial.Cons.Append
                    (Constructor_Pattern'
                       (Pat.Constructor_Args'Length, Rest.Pat_Count,
                        Pat.Location, Core.To_Conid (Pat.Constructor_Name),
                        [for Arg of Args => Core.To_Varid (Arg.Variable_Name)],
                        Rest));
               end;
            elsif Pat.Is_Literal then
               Partial.Literals.Append
                 (Literal_Pattern'
                    (Rest.Pat_Count, Pat.Location, Pat.To_Core, Rest));
            else
               raise Constraint_Error with
                 "unknown pattern type";
            end if;
         end;
      end loop;

      declare
         Count : constant Natural :=
                   Natural (Partial.Cons.Length)
                   + Natural (Partial.Literals.Length)
                   + (if Partial.Vars.Is_Empty
                      then 0
                      else 1);
         Last  : Natural := 0;
         Alts  : Core.Alts.Reference_Array (1 .. Count);
      begin
         for Con of Partial.Cons loop
            Last := Last + 1;
            Alts (Last) := To_Core_Alt (Con);
         end loop;
         for Lit of Partial.Literals loop
            Last := Last + 1;
            Alts (Last) := To_Core_Alt (Lit);
         end loop;
         if not Partial.Vars.Is_Empty then
            Last := Last + 1;
            Alts (Last) := To_Core_Alt (Partial.Vars.First_Element.Location,
                                        Partial.Vars);
         end if;
         return Alts;
      end;
   end To_Multipattern_Alts;

end Leander.Syntax.Bindings.Transform;
