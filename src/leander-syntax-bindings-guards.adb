with Ada.Strings.Unbounded;

with Leander.Syntax.Expressions;

package body Leander.Syntax.Bindings.Guards is

   use Ada.Strings.Unbounded;

   function Can_Fail (Equation : Binding_Record) return Boolean
   is (Length (Equation.Fallthrough) > 0);

   function Let_Be
     (Loc    : Source.Source_Location;
      Name   : String;
      Value  : Expression_Reference;
      Within : Expression_Reference)
      return Expression_Reference;

   function Fail_Call
     (Loc     : Source.Source_Location;
      Message : String)
      return Expression_Reference;

   ---------------
   -- Fail_Call --
   ---------------

   function Fail_Call
     (Loc     : Source.Source_Location;
      Message : String)
      return Expression_Reference
   is (Expression_Reference
         (Leander.Syntax.Expressions.Application
            (Loc,
             Leander.Syntax.Expressions.Variable (Loc, "error"),
             Leander.Syntax.Expressions.String_Literal (Loc, Message))));

   ------------
   -- Let_Be --
   ------------

   function Let_Be
     (Loc    : Source.Source_Location;
      Name   : String;
      Value  : Expression_Reference;
      Within : Expression_Reference)
      return Expression_Reference
   is
      Group : constant Reference := Empty (Monomorphic => True);
   begin
      --  Monomorphic: the group is synthesised and used at exactly one
      --  site, and generalising it would strand any class constraint the
      --  guards raise (issue #70).
      Group.Add_Binding (Loc, Name, [], Value);
      return Expression_Reference
        (Leander.Syntax.Expressions.Let
           (Loc, Group, Leander.Syntax.Expressions.Reference (Within)));
   end Let_Be;

   -----------
   -- Lower --
   -----------

   procedure Lower
     (Name      : Leander.Names.Leander_Name;
      Equations : in out Binding_Record_Lists.List)
   is
      Count     : constant Natural := Natural (Equations.Length);
      Arity     : constant Natural := Equations.First_Element.Pat_Count;
      Loc       : constant Source.Source_Location :=
                    Equations.First_Element.Expr.Location;
      Label     : constant String := Leander.Names.To_String (Name);
      Any_Fails : Boolean := False;
      Staged    : Boolean := False;
      Index     : Natural := 0;
   begin
      for Equation of Equations loop
         Index := Index + 1;
         if Can_Fail (Equation) then
            Any_Fails := True;

            --  Something follows that a failed guard must still try, and
            --  the alts compiler has already committed by then.  Arity
            --  zero cannot be staged -- there is nothing to re-dispatch
            --  on, and the equation would become a lambda-form binding,
            --  which gets no Y and so would break recursion -- but a name
            --  with no parameters has only the one equation anyway.
            if Index /= Count and then Arity > 0 then
               Staged := True;
            end if;
         end if;
      end loop;

      if not Any_Fails then
         return;
      end if;

      if not Staged then

         --  Nothing follows any failing equation, so a failed guard has
         --  run out of alternatives.  Each equation keeps its shape and
         --  binds its own fallthrough locally, which leaves the alts
         --  compiler seeing exactly what it would see without guards.
         for Equation of Equations loop
            if Can_Fail (Equation) then
               declare
                  E : Binding_Record := Equation;
               begin
                  E.Expr := Let_Be
                    (Loc, To_String (E.Fallthrough),
                     Fail_Call (Loc, "Non-exhaustive guards in " & Label),
                     E.Expr);
                  E.Fallthrough := Null_Unbounded_String;
                  Equation := E;
               end;
            end if;
         end loop;
         return;
      end if;

      --  Stage the group.  Each equation becomes one nullary thunk that
      --  dispatches on the same, already evaluated, argument variables and
      --  falls out to the next thunk, so a failed guard costs one further
      --  dispatch and never re-evaluates the scrutinee:
      --
      --    f v1 .. vm = let k_n+1 = error "..." in
      --                 ...
      --                 let k_1 = <equation 1, falling out to k_2> in
      --                 k_1
      --
      --  The thunks are nested explicitly rather than collected into one
      --  binding group because a let wraps its bindings outward as it
      --  emits them: the last one named ends up outermost, and so is the
      --  only one the others can see.  Nesting them here keeps that order
      --  ours rather than the dependency sort's.
      --
      --  The equation that replaces the group keeps its parameters as
      --  patterns.  Lowering to "f = \v1 -> ..." instead would leave a
      --  binding with no patterns, and that path adds no Y, so every
      --  recursive guarded function would silently stop resolving.
      declare
         Vars : constant Leander.Names.Name_Array (1 .. Arity) :=
                  [for I in 1 .. Arity => Leander.Names.New_Name];
         Ks   : constant Leander.Names.Name_Array (1 .. Count + 1) :=
                  [for I in 1 .. Count + 1 => Leander.Names.New_Name];
         Pats : constant Patterns.Reference_Array (1 .. Arity) :=
                  [for I in 1 .. Arity =>
                     Patterns.Variable
                       (Loc, Leander.Names.To_String (Vars (I)))];

         function K (I : Positive) return Expression_Reference
         is (Expression_Reference
               (Leander.Syntax.Expressions.Variable
                  (Loc, Leander.Names.To_String (Ks (I)))));

         function Stage
           (Equation : Binding_Record;
            Next     : Positive)
            return Expression_Reference;

         -----------
         -- Stage --
         -----------

         function Stage
           (Equation : Binding_Record;
            Next     : Positive)
            return Expression_Reference
         is
            Group   : constant Reference := Empty (Monomorphic => True);
            Matcher : constant String :=
                        Leander.Names.To_String (Leander.Names.New_Name);
            Applied : Leander.Syntax.Expressions.Reference :=
                        Leander.Syntax.Expressions.Variable (Loc, Matcher);
            Matched : Expression_Reference := Equation.Expr;
            Total   : Boolean := True;
         begin
            if Can_Fail (Equation) then
               Matched := Let_Be
                 (Loc, To_String (Equation.Fallthrough), K (Next), Matched);
            end if;

            Group.Add_Binding (Loc, Matcher, Equation.Pats, Matched);

            for Pat of Equation.Pats loop
               if not Pat.Is_Variable then
                  Total := False;
               end if;
            end loop;

            --  A pattern that can fail to match needs the same exit as a
            --  guard that fails.  One that cannot would only make the
            --  alternative unreachable.
            if not Total then
               Group.Add_Binding
                 (Loc, Matcher,
                  [for I in 1 .. Arity => Patterns.Wildcard (Loc)],
                  K (Next));
            end if;

            for I in 1 .. Arity loop
               Applied := Leander.Syntax.Expressions.Application
                 (Loc, Applied,
                  Leander.Syntax.Expressions.Variable
                    (Loc, Leander.Names.To_String (Vars (I))));
            end loop;

            return Expression_Reference
              (Leander.Syntax.Expressions.Let (Loc, Group, Applied));
         end Stage;

         Stages : array (1 .. Count) of Expression_Reference;
         Chain  : Expression_Reference := K (1);
      begin
         Index := 0;
         for Equation of Equations loop
            Index := Index + 1;
            Stages (Index) := Stage (Equation, Index + 1);
         end loop;

         --  Wrap outwards, so that each stage is bound inside the one it
         --  falls out to: stage I names K (I + 1), so K (I + 1) has to be
         --  bound further out than K (I), and the failure case furthest
         --  out of all.
         for I in 1 .. Count loop
            Chain := Let_Be
              (Loc, Leander.Names.To_String (Ks (I)), Stages (I), Chain);
         end loop;

         Chain := Let_Be
           (Loc, Leander.Names.To_String (Ks (Count + 1)),
            Fail_Call (Loc, "Non-exhaustive patterns in " & Label),
            Chain);

         Equations.Clear;
         Equations.Append
           (Binding_Record'
              (Pat_Count   => Arity,
               Pats        => Pats,
               Expr        => Chain,
               Fallthrough => Null_Unbounded_String));
      end;
   end Lower;

end Leander.Syntax.Bindings.Guards;
