with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;

with Leander.Syntax.Expressions;

with WL.String_Sets;

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

   -------------
   -- Is_Flat --
   -------------

   function Is_Flat (Pat : Patterns.Reference) return Boolean
   is (Pat.Is_Variable or else Pat.Is_Literal
       or else (for all Arg of Pat.Constructor_Args => Arg.Is_Variable));
   --  Whether Pat is what To_Alts and the alts compiler take: a variable,
   --  a literal, or a constructor whose arguments are all variables.

   ---------------
   -- Groupable --
   ---------------

   function Groupable (Equations : Binding_Record_Lists.List) return Boolean
   is
      Arity    : constant Natural :=
                   (if Equations.Is_Empty then 0
                    else Equations.First_Element.Pat_Count);
      Has_Var  : constant Boolean :=
                   Arity > 0
                   and then (for some E of Equations =>
                               E.Pats (1).Is_Variable);
      Seen_Var : Boolean := False;
      Keys     : WL.String_Sets.Set;
      Rest     : Binding_Record_Lists.List;
   begin
      --  Transform.To_Alts dispatches on the first column once, giving each
      --  constructor or literal its own alternative and every equation
      --  that starts with a variable one more, and then matches the rest
      --  of the columns within those.  That is right only when it cannot
      --  change which equation applies: no first-column constructor or
      --  literal twice (only the first would be kept), none after an
      --  equation that starts with a variable (it would be tried first),
      --  and none whose later columns can fail while a variable equation
      --  could still take the value (nothing would fall through to it).
      --  The same goes for the variable equations' own later columns.
      --  One column is always right: the alts compiler stops at the first
      --  variable pattern itself (issue #120).
      if Arity <= 1 then
         return True;
      end if;

      for E of Equations loop
         declare
            Pat : constant Patterns.Reference := E.Pats (1);
         begin
            if Pat.Is_Variable then
               Seen_Var := True;
               Rest.Append
                 (Binding_Record'
                    (Pat_Count   => E.Pat_Count - 1,
                     Pats        => E.Pats (2 .. E.Pat_Count),
                     Expr        => E.Expr,
                     Fallthrough => E.Fallthrough));
            else
               declare
                  Key : constant String :=
                          (if Pat.Is_Constructor
                           then "C" & Pat.Constructor_Name
                           else "L" & Pat.Literal);
               begin
                  if Seen_Var or else Keys.Contains (Key) then
                     return False;
                  end if;
                  Keys.Include (Key);
               end;

               if Has_Var
                 and then (for some I in 2 .. E.Pat_Count =>
                             not E.Pats (I).Is_Variable)
               then
                  return False;
               end if;
            end if;
         end;
      end loop;

      return Groupable (Rest);
   end Groupable;

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

      --  Equations that To_Alts cannot dispatch on in one go are staged
      --  too, so that each is tried in its turn (issue #120).
      if Count > 1 and then Arity > 0 and then not Groupable (Equations) then
         Staged := True;
      end if;

      --  So are equations with a nested pattern, even just one: staging
      --  matches the inner pattern as a further step (issue #109).
      if Arity > 0
        and then (for some E of Equations =>
                    (for some Pat of E.Pats => not Is_Flat (Pat)))
      then
         Staged := True;
      end if;

      if not Any_Fails and then not Staged then
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
            type Check is
               record
                  Var : Ada.Strings.Unbounded.Unbounded_String;
                  Pat : Patterns.Reference;
               end record;

            package Check_Vectors is
              new Ada.Containers.Vectors (Positive, Check);

            Checks  : Check_Vectors.Vector;
            Matched : Expression_Reference := Equation.Expr;
         begin
            if Can_Fail (Equation) then
               Matched := Let_Be
                 (Loc, To_String (Equation.Fallthrough), K (Next), Matched);
            end if;

            --  A parameter is matched against its pattern.  A pattern with
            --  a constructor argument that is not a variable matches that
            --  argument with a fresh variable instead, and the variable is
            --  then matched against the argument as a further check, after
            --  the one that binds it -- (x:(y:_)) is (x:v) and then v
            --  against (y:_) (issue #109).
            for I in 1 .. Arity loop
               Checks.Append
                 (Check'(To_Unbounded_String (Leander.Names.To_String (Vars (I))),
                   Equation.Pats (I)));
            end loop;

            declare
               Index : Positive := 1;
            begin
               while Index <= Checks.Last_Index loop
                  declare
                     Pat : constant Patterns.Reference := Checks (Index).Pat;
                  begin
                     if not Is_Flat (Pat) then
                        declare
                           Args : constant Patterns.Reference_Array :=
                                    Pat.Constructor_Args;
                           Flat : Patterns.Reference_Array (Args'Range);
                        begin
                           for J in Args'Range loop
                              if Args (J).Is_Variable then
                                 Flat (J) := Args (J);
                              else
                                 declare
                                    Fresh : constant String :=
                                              Leander.Names.To_String
                                                (Leander.Names.New_Name);
                                 begin
                                    Flat (J) :=
                                      Patterns.Variable (Pat.Location, Fresh);
                                    Checks.Append
                                      (Check'(To_Unbounded_String (Fresh),
                                        Args (J)));
                                 end;
                              end if;
                           end loop;
                           Checks (Index).Pat :=
                             Patterns.Constructor
                               (Pat.Location, Pat.Constructor_Name, Flat);
                        end;
                     end if;
                  end;
                  Index := Index + 1;
               end loop;
            end;

            --  One column at a time, the first outermost, so that the
            --  variables each binds are in scope for the rest:
            --
            --    let m1 p1 = (let m2 p2 = ... in m2 v2); m1 _ = k_next
            --    in m1 v1
            --
            --  A single column with one pattern and a default is exactly
            --  what the alts compiler gets right, wherever a later column
            --  fails.  Matching every column together instead could not
            --  fall out of a later one (issue #120).
            for C of reverse Checks loop
               declare
                  Pat     : constant Patterns.Reference := C.Pat;
                  Group   : constant Reference := Empty (Monomorphic => True);
                  Matcher : constant String :=
                              Leander.Names.To_String (Leander.Names.New_Name);
               begin
                  Group.Add_Binding (Loc, Matcher, [Pat], Matched);

                  --  A pattern that can fail to match needs the same exit
                  --  as a guard that fails.  One that cannot would only
                  --  make the alternative unreachable.
                  if not Pat.Is_Variable then
                     Group.Add_Binding
                       (Loc, Matcher, [Patterns.Wildcard (Loc)], K (Next));
                  end if;

                  Matched := Expression_Reference
                    (Leander.Syntax.Expressions.Let
                       (Loc, Group,
                        Leander.Syntax.Expressions.Application
                          (Loc,
                           Leander.Syntax.Expressions.Variable
                             (Loc, Matcher),
                           Leander.Syntax.Expressions.Variable
                             (Loc, To_String (C.Var)))));
               end;
            end loop;

            return Matched;
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
