with Ada.Exceptions;

with Leander.Allocator;
with Leander.Core.Binding_Groups;
with Leander.Core.Bindings.Dependencies;
with Leander.Core.Predicates;
with Leander.Core.Substitutions;
with Leander.Core.Type_Instances;
with Leander.Core.Types;
with Leander.Environment;

package body Leander.Core.Expressions is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("expressions", Instance, Variable_Reference);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance)
      return Reference
   is
   begin
      return Reference (Allocator.Allocate (This));
   end Allocate;

   -----------------
   -- Application --
   -----------------

   function Application
     (Loc         : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference
   is
   begin
      return Allocate
        (Instance'(EApp, Core.Typeable.New_Id, Loc, null, Left, Right));
   end Application;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (Loc     : Leander.Source.Source_Location;
      Id      : Conid)
      return Reference
   is
   begin
      return Allocate ((ECon, Core.Typeable.New_Id, Loc, null, Id));
   end Constructor;

   -------------
   -- Dispose --
   -------------

   overriding procedure Dispose (This : in out Instance) is
   begin
      null;
   end Dispose;

   -------------------
   -- Has_Reference --
   -------------------

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean
   is
   begin
      case This.Tag is
         when EVar =>
            return This.Var_Id = To;
         when ECon =>
            return False;
         when ELit =>
            return False;
         when EApp =>
            return This.Left.Has_Reference (To)
              or else This.Right.Has_Reference (To);
         when ELam =>
            return This.LVar /= To
              and then This.LBody.Has_Reference (To);
         when ELet =>
            return This.Let_Bindings.Has_Reference (To)
              or else This.Let_Body.Has_Reference (To);
      end case;
   end Has_Reference;

   ------------
   -- Lambda --
   ------------

   function Lambda (Loc        : Leander.Source.Source_Location;
      Id         : Varid;
      Expression : Reference)
                    return Reference
   is
   begin
      return Allocate
        ((ELam, Core.Typeable.New_Id, Loc, null, Id, Expression));
   end Lambda;

   ---------
   -- Let --
   ---------

   function Let
     (Loc      : Leander.Source.Source_Location;
      Bindings : Leander.Core.Binding_Groups.Reference;
      Expr     : Reference)
      return Reference
   is
      Ref : constant Binding_Group_Reference :=
              Binding_Group_Reference (Bindings);
   begin
      return Allocate ((ELet, Core.Typeable.New_Id, Loc, null, Ref, Expr));
   end Let;

   -------------
   -- Literal --
   -------------

   function Literal (Loc : Leander.Source.Source_Location;
      Lit : Literals.Instance)
                     return Reference is
   begin
      return Allocate ((ELit, Core.Typeable.New_Id, Loc, null, Lit));
   end Literal;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

   ------------
   -- Report --
   ------------

   procedure Report is
   begin
      Allocator.Report;
   end Report;

   ------------------------
   -- Set_Qualified_Type --
   ------------------------

   overriding procedure Set_Qualified_Type
     (This : in out Instance;
      QT   : Leander.Core.Qualified_Types.Reference)
   is
   begin
      This.QT := Nullable_Qualified_Type_Reference (QT);
   end Set_Qualified_Type;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
   begin
      case This.Tag is
         when EVar =>
            return To_String (This.Var_Id);
         when ECon =>
            return To_String (This.Con_Id);
         when ELit =>
            return This.Literal.Show;
         when EApp =>
            declare
               function Paren (Img : String; P : Boolean) return String
               is (if P then "(" & Img & ")" else Img);

               Left_Image : constant String :=
                              Paren (This.Left.Show,
                                     This.Left.Tag in ELam | ELet);

               Right_Image : constant String :=
                               Paren (This.Right.Show,
                                      This.Right.Tag in EApp | ELam | ELet);
            begin
               return Left_Image & " " & Right_Image;
            end;
         when ELam =>
            return "\" & To_String (This.LVar) & " -> "
              & This.LBody.Show;
         when ELet =>
            return "let {" & This.Let_Bindings.Show
              & "} in " & This.Let_Body.Show;
      end case;
   end Show;

   ---------------
   -- Dict_Expr --
   ---------------

   function Dict_Expr
     (Env : not null access constant Leander.Environment.Abstraction'Class;
      P   : Leander.Core.Predicates.Instance)
      return Leander.Calculus.Tree
   is
      use Leander.Calculus;
      Instances : constant Leander.Core.Type_Instances.Reference_Array :=
                    Env.All_Instances (P.Class_Id);
   begin
      if P.Get_Type.all.Head_Normal_Form then
         return Symbol ("<" & P.Show & ">");
      end if;
      for Inst of Instances loop
         declare
            Success : Boolean;
            Subst   : constant Leander.Core.Substitutions.Instance :=
                        Leander.Core.Types.Match
                          (Inst.Predicate.Get_Type, P.Get_Type, Success);
         begin
            if Success then
               declare
                  Sub_Ps : constant Leander.Core.Predicates.Predicate_Array :=
                             Inst.Qualifier.all.Apply (Subst).all.Predicates;
                  E      : Tree :=
                             Symbol ("<" & Inst.Predicate.Show & ">");
               begin
                  for Sub_P of Sub_Ps loop
                     E := Apply (E, Dict_Expr (Env, Sub_P));
                  end loop;
                  return E;
               end;
            end if;
         end;
      end loop;
      return Symbol ("<" & P.Show & ">");
   end Dict_Expr;

   ---------------
   -- Dict_Name --
   ---------------

   function Dict_Name
     (Types : Leander.Core.Inference.Inference_Context'Class;
      P     : Leander.Core.Predicates.Instance)
      return String
   is
      --  Resolve the predicate against the substitution as it now stands.
      --  Generalisation records a binding's dictionaries before the enclosing
      --  binding is finished, so a variable still free then may since have
      --  been resolved -- and the body's own references, rewritten by
      --  Update_Type, name the dictionary by its resolved type.
      Q : constant Leander.Core.Predicates.Instance :=
            Leander.Core.Predicates.Predicate
              (P.Class_Id,
               Leander.Core.Types.Reference
                 (P.Get_Type.Apply (Types.Current_Substitution)));
   begin
      return "<" & Q.Show & ">";
   end Dict_Name;

   -----------------
   -- To_Calculus --
   -----------------

   function To_Calculus
     (This  : Instance'Class;
      Types : in out Leander.Core.Inference.Inference_Context'Class;
      Env   : not null access constant Leander.Environment.Abstraction'Class)
      return Leander.Calculus.Tree
   is
      use Leander.Calculus;
      Result : Tree;
   begin
      case This.Tag is
         when EVar =>
            declare
               E : Tree :=
                     Symbol (Leander.Names.Leander_Name (This.Var_Id));
            begin
               if This.Has_Qualified_Type_Value then
                  declare
                     Ps : constant Leander.Core.Predicates.Predicate_Array :=
                            This.Qualified_Type.Predicates;
                  begin
                     for P of Ps loop
                        E := Apply (E, Dict_Expr (Env, P));
                     end loop;
                     Types.Save_Predicates (Ps);
                  end;
               end if;
               Result := E;
            end;
         when ECon =>
            Result := Env.Constructor (Leander.Names.Leander_Name (This.Con_Id));
         when ELit =>
            Result := This.Literal.To_Calculus;
         when EApp =>
            Result := Apply
              (This.Left.To_Calculus (Types, Env),
               This.Right.To_Calculus (Types, Env));
         when ELam =>
            Result := Lambda
              (To_String (This.LVar),
               This.LBody.To_Calculus (Types, Env));
         when ELet =>
            declare
               Bs : constant Leander.Core.Bindings.Reference_Array :=
                      [for Id of This.Let_Bindings.Varids =>
                         This.Let_Bindings.Lookup
                           (Leander.Names.Leander_Name (Id))];
               Component : constant Bindings.Dependencies.Component_Array :=
                             Bindings.Dependencies.Components (Bs);

               function Binding_Calculus
                 (B             : Leander.Core.Bindings.Reference;
                  Tie_Recursion : Boolean)
                  return Tree;
               --  B, wrapped in its dictionary lambdas.  With Tie_Recursion
               --  its own name is bound by Y; otherwise it is left free.

               function Projection
                 (Index, Count : Positive)
                  return Tree;
               --  \x1 .. xCount. xIndex, which selects field Index of a
               --  Scott-encoded tuple.

               function Bind_Members
                 (Members : Leander.Core.Bindings.Reference_Array;
                  Tuple   : Leander.Names.Leander_Name;
                  Scope   : Tree)
                  return Tree;
               --  (\m1 .. mN. Scope) (Tuple pi1) .. (Tuple piN), binding
               --  each member's name in Scope to its own field of Tuple.

               ----------------------
               -- Binding_Calculus --
               ----------------------

               function Binding_Calculus
                 (B             : Leander.Core.Bindings.Reference;
                  Tie_Recursion : Boolean)
                  return Tree
               is
                  Dicts : constant Leander.Core.Predicates.Predicate_Array :=
                            B.Dictionaries;
                  Base  : constant Natural := Types.Predicate_Count;
                  Calc  : Tree :=
                            B.To_Calculus
                              (Types, Env,
                               Tie_Recursion =>
                                 Tie_Recursion and then not B.Is_Explicit);
               begin
                  --  Left untied, an implicit binding's group mates (itself
                  --  included) are free, and will be bound to their whole
                  --  values, dictionary lambdas and all.  But it uses them
                  --  monomorphically, applying no dictionaries, so within
                  --  it each must instead denote that value applied to the
                  --  dictionaries the whole group shares.
                  if not Tie_Recursion
                    and then not B.Is_Explicit
                    and then Dicts'Length > 0
                  then
                     declare
                        Mates : constant
                          Leander.Core.Bindings.Reference_Array :=
                            This.Let_Bindings.Implicit_Group (B.Name);
                     begin
                        for M of reverse Mates loop
                           Calc :=
                             Lambda (Leander.Names.Leander_Name (M.Name), Calc);
                        end loop;
                        for M of Mates loop
                           declare
                              Applied : Tree :=
                                          Symbol
                                            (Leander.Names.Leander_Name
                                               (M.Name));
                           begin
                              for P of Dicts loop
                                 Applied :=
                                   Apply
                                     (Applied,
                                      Symbol (Dict_Name (Types, P)));
                              end loop;
                              Calc := Apply (Calc, Applied);
                           end;
                        end loop;
                     end;
                  end if;

                  --  One dictionary lambda per predicate the binding's
                  --  scheme retains.  Wrapped in reverse, so the outermost
                  --  parameter is the first dictionary a use site applies
                  --  (the EVar case above applies them in scheme order).
                  for P of reverse Dicts loop
                     Calc := Lambda (Dict_Name (Types, P), Calc);
                  end loop;

                  --  An explicit binding's recursive uses apply those
                  --  dictionaries themselves, so its name is bound
                  --  outside them (see Bindings.To_Calculus).
                  if Tie_Recursion
                    and then B.Is_Explicit
                    and then B.Is_Recursive
                  then
                     Calc := B.Tie (Calc);
                  end if;

                  --  Compiling the body re-raised its predicates into the
                  --  context.  The ones this binding just took as its own
                  --  parameters are discharged here and must not travel
                  --  further out, or whatever encloses this expression
                  --  would wrap itself in a dictionary lambda for them
                  --  that nothing ever supplies.  Anything else the body
                  --  raised is genuinely deferred, so it is put back.
                  declare
                     Raised : constant
                       Leander.Core.Predicates.Predicate_Array :=
                         Types.Current_Predicates;
                     Keep   : Leander.Core.Predicates.Predicate_Array
                       (1 .. Raised'Last - Base);
                     Last   : Natural := 0;
                  begin
                     for K in Base + 1 .. Raised'Last loop
                        if (for all D of Dicts =>
                              Dict_Name (Types, D)
                                /= Dict_Name (Types, Raised (K)))
                        then
                           Last := Last + 1;
                           Keep (Last) := Raised (K);
                        end if;
                     end loop;
                     Types.Drop_Predicates (Base + 1);
                     Types.Save_Predicates (Keep (1 .. Last));
                  end;

                  return Calc;
               end Binding_Calculus;

               ------------------
               -- Bind_Members --
               ------------------

               function Bind_Members
                 (Members : Leander.Core.Bindings.Reference_Array;
                  Tuple   : Leander.Names.Leander_Name;
                  Scope   : Tree)
                  return Tree
               is
                  Result : Tree := Scope;
               begin
                  for M of reverse Members loop
                     Result :=
                       Lambda (Leander.Names.Leander_Name (M.Name), Result);
                  end loop;
                  for I in Members'Range loop
                     Result :=
                       Apply
                         (Result,
                          Apply
                            (Symbol (Tuple),
                             Projection
                               (I - Members'First + 1, Members'Length)));
                  end loop;
                  return Result;
               end Bind_Members;

               ----------------
               -- Projection --
               ----------------

               function Projection
                 (Index, Count : Positive)
                  return Tree
               is
                  Names  : constant Leander.Names.Name_Array (1 .. Count) :=
                             [others => Leander.Names.New_Name];
                  Result : Tree := Symbol (Names (Index));
               begin
                  for Name of reverse Names loop
                     Result := Lambda (Name, Result);
                  end loop;
                  return Result;
               end Projection;

               E : Tree := This.Let_Body.To_Calculus (Types, Env);
            begin
               --  A binding sees only the bindings nested outside it, so
               --  the dependency components are nested with each one
               --  outside everything that refers to it: the last component
               --  innermost.  Bindings that refer to each other share a
               --  component, and are compiled together as a recursive tuple.
               for C in reverse 1 .. Bindings.Dependencies.Component_Count
                                       (Component)
               loop
                  declare
                     Members : Leander.Core.Bindings.Reference_Array
                       (1 .. Component'Length);
                     Count   : Natural := 0;
                  begin
                     for I in Bs'Range loop
                        if Component (I) = C then
                           Count := Count + 1;
                           Members (Count) := Bs (I);
                        end if;
                     end loop;

                     if Count = 1 then
                        E := Apply
                          (Lambda
                             (Leander.Names.Leander_Name (Members (1).Name),
                              E),
                           Binding_Calculus
                             (Members (1), Tie_Recursion => True));
                     else
                        --  let m1 = e1; .. mN = eN in E, all mutually
                        --  recursive, becomes
                        --
                        --    (\t. (\m1 .. mN. E) (t pi1) .. (t piN))
                        --      (Y (\t. (\m1 .. mN. \s. s e1 .. eN)
                        --                (t pi1) .. (t piN)))
                        --
                        --  where t is the tuple of all N bindings.
                        declare
                           Group    : Leander.Core.Bindings.Reference_Array
                             renames Members (1 .. Count);
                           Tuple    : constant Leander.Names.Leander_Name :=
                                        Leander.Names.New_Name;
                           Selector : constant Leander.Names.Leander_Name :=
                                        Leander.Names.New_Name;
                           Fields   : Tree := Symbol (Selector);
                        begin
                           for M of Group loop
                              Fields :=
                                Apply
                                  (Fields,
                                   Binding_Calculus
                                     (M, Tie_Recursion => False));
                           end loop;

                           E := Apply
                             (Lambda (Tuple, Bind_Members (Group, Tuple, E)),
                              Apply
                                (Symbol ("Y"),
                                 Lambda
                                   (Tuple,
                                    Bind_Members
                                      (Group, Tuple,
                                       Lambda (Selector, Fields)))));
                        end;
                     end if;
                  end;
               end loop;
               Result := E;
            end;
      end case;
      return Result;

   exception
      when E : others =>
         This.Error (Ada.Exceptions.Exception_Message (E));
         raise;
   end To_Calculus;

   --------------
   -- Traverse --
   --------------

   overriding procedure Traverse
     (This : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Traverseable.Abstraction'Class))
   is
   begin
      Process (This);
      case This.Tag is
         when EVar =>
            null;
         when ECon =>
            null;
         when ELit =>
            null;
         when EApp =>
            This.Left.Traverse (Process);
            This.Right.Traverse (Process);
         when ELam =>
            This.LBody.Traverse (Process);
         when ELet =>
            for Id of This.Let_Bindings.Varids loop
               for Alt of
                 This.Let_Bindings.Lookup (Leander.Names.Leander_Name (Id))
                   .Alts
               loop
                  Alt.Traverse (Process);
               end loop;
            end loop;
            This.Let_Body.Traverse (Process);
      end case;
   end Traverse;

   ---------------------
   -- Update_Traverse --
   ---------------------

   overriding procedure Update_Traverse
     (This    : not null access Instance;
      Process : not null access
        procedure (This : not null access
                     Leander.Traverseable.Abstraction'Class))
   is
   begin
      Process (This);
      case This.Tag is
         when EVar =>
            null;
         when ECon =>
            null;
         when ELit =>
            null;
         when EApp =>
            This.Left.Update_Traverse (Process);
            This.Right.Update_Traverse (Process);
         when ELam =>
            This.LBody.Update_Traverse (Process);
         when ELet =>
            for Id of This.Let_Bindings.Varids loop
               for Alt of
                 This.Let_Bindings.Lookup (Leander.Names.Leander_Name (Id))
                   .Alts
               loop
                  Alt.Update_Traverse (Process);
               end loop;
            end loop;
            This.Let_Body.Update_Traverse (Process);
      end case;
   end Update_Traverse;

   --------------
   -- Variable --
   --------------

   function Variable (Loc : Leander.Source.Source_Location;
      Id  : Varid)
                      return Reference is
   begin
      return Allocate (Instance'(EVar, Core.Typeable.New_Id, Loc, null, Id));
   end Variable;

end Leander.Core.Expressions;
