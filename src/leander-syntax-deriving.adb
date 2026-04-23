with Leander.Core;
with Leander.Core.Predicates;
with Leander.Core.Schemes;
with Leander.Core.Tyvars;
with Leander.Core.Types;

with Leander.Names;

with Leander.Syntax.Bindings;
with Leander.Syntax.Expressions;
with Leander.Syntax.Patterns;

package body Leander.Syntax.Deriving is

   function Con_Arity
     (Scheme : Leander.Core.Schemes.Reference)
      return Natural;

   ----------------
   -- Con_Arity --
   ----------------

   function Con_Arity
     (Scheme : Leander.Core.Schemes.Reference)
      return Natural
   is
      use type Leander.Core.Types.Reference;
      T     : Leander.Core.Types.Reference := Scheme.Inner_Type;
      Count : Natural := 0;
   begin
      while T.Is_Application
        and then T.Left.Is_Application
        and then T.Left.Left = Leander.Core.Types.T_Arrow
      loop
         Count := Count + 1;
         T := T.Right;
      end loop;
      return Count;
   end Con_Arity;

   ---------------
   -- Derive_Eq --
   ---------------

   procedure Derive_Eq
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty
                     (Leander.Core.Instance_Context, []);

      Tyvars   : constant Leander.Core.Tyvars.Tyvar_Array :=
                   DT.Applied_Type.Get_Tyvars;

      function Var_Expr (Name : String)
         return Leander.Syntax.Expressions.Reference
      is (Leander.Syntax.Expressions.Variable (Loc, Name));

      function Con_Expr (Name : String)
         return Leander.Syntax.Expressions.Reference
      is (Leander.Syntax.Expressions.Constructor (Loc, Name));

      function App
        (F, X : Leander.Syntax.Expressions.Reference)
         return Leander.Syntax.Expressions.Reference
      is (Leander.Syntax.Expressions.Application (Loc, F, X));

      function Op
        (Name : String;
         L, R : Leander.Syntax.Expressions.Reference)
         return Leander.Syntax.Expressions.Reference
      is (App (App (Var_Expr (Name), L), R));

      function Pat_Var (Name : String)
         return Leander.Syntax.Patterns.Reference
      is (Leander.Syntax.Patterns.Variable (Loc, Name));

      function Pat_Con
        (Name : String;
         Args : Leander.Syntax.Patterns.Reference_Array)
         return Leander.Syntax.Patterns.Reference
      is (Leander.Syntax.Patterns.Constructor (Loc, Name, Args));

   begin
      for I in 1 .. DT.Constructor_Count loop
         declare
            I_Name   : constant String :=
                         Leander.Core.To_String (DT.Constructor_Name (I));
            I_Arity  : constant Natural :=
                         Con_Arity (DT.Constructor_Type (I));
            L_Args   : Leander.Syntax.Patterns.Reference_Array (1 .. I_Arity);
            L_Names  : Leander.Names.Name_Array (1 .. I_Arity);
            Y_Name   : constant String :=
                         Leander.Names.To_String (Leander.Names.New_Name);
            Fn_Name  : constant String :=
                         Leander.Names.To_String (Leander.Names.New_Name);
            Inner_Bs : constant Leander.Syntax.Bindings.Reference :=
                         Leander.Syntax.Bindings.Empty;
         begin
            for K in 1 .. I_Arity loop
               L_Names (K) := Leander.Names.New_Name;
               L_Args (K) := Pat_Var (Leander.Names.To_String (L_Names (K)));
            end loop;

            for J in 1 .. DT.Constructor_Count loop
               declare
                  J_Name  : constant String :=
                              Leander.Core.To_String
                                (DT.Constructor_Name (J));
                  J_Arity : constant Natural :=
                              Con_Arity (DT.Constructor_Type (J));
                  J_Args  : Leander.Syntax.Patterns.Reference_Array
                              (1 .. J_Arity);
                  J_Names : Leander.Names.Name_Array (1 .. J_Arity);
                  Body_E  : Leander.Syntax.Expressions.Reference;
               begin
                  for K in 1 .. J_Arity loop
                     J_Names (K) := Leander.Names.New_Name;
                     J_Args (K) := Pat_Var
                       (Leander.Names.To_String (J_Names (K)));
                  end loop;

                  if I = J then
                     if I_Arity = 0 then
                        Body_E := Con_Expr ("True");
                     else
                        Body_E := Op
                          ("==",
                           Var_Expr
                             (Leander.Names.To_String (L_Names (I_Arity))),
                           Var_Expr
                             (Leander.Names.To_String (J_Names (I_Arity))));
                        for K in reverse 1 .. I_Arity - 1 loop
                           Body_E := Op
                             ("&&",
                              Op ("==",
                                  Var_Expr
                                    (Leander.Names.To_String (L_Names (K))),
                                  Var_Expr
                                    (Leander.Names.To_String (J_Names (K)))),
                              Body_E);
                        end loop;
                     end if;
                  else
                     Body_E := Con_Expr ("False");
                  end if;

                  Inner_Bs.Add_Binding
                    (Loc, Fn_Name,
                     [Pat_Con (J_Name, J_Args)],
                     Body_E);
               end;
            end loop;

            declare
               Case_Expr : constant Leander.Syntax.Expressions.Reference :=
                             Leander.Syntax.Expressions.Let
                               (Loc, Inner_Bs,
                                App (Var_Expr (Fn_Name),
                                     Var_Expr (Y_Name)));

               LHS : constant Leander.Syntax.Bindings.Binding_LHS :=
                       Leander.Syntax.Bindings.Create_Binding_LHS
                         ("==",
                          [Pat_Con (I_Name, L_Args),
                           Pat_Var (Y_Name)]);
            begin
               Bindings.Add_Binding (Loc, LHS, Case_Expr);
            end;
         end;
      end loop;

      declare
         X_Name : constant String :=
                    Leander.Names.To_String (Leander.Names.New_Name);
         Y_Name : constant String :=
                    Leander.Names.To_String (Leander.Names.New_Name);
         LHS    : constant Leander.Syntax.Bindings.Binding_LHS :=
                    Leander.Syntax.Bindings.Create_Binding_LHS
                      ("/=",
                       [Pat_Var (X_Name), Pat_Var (Y_Name)]);
         RHS    : constant Leander.Syntax.Expressions.Reference :=
                    App (Var_Expr ("not"),
                         Op ("==",
                             Var_Expr (X_Name),
                             Var_Expr (Y_Name)));
      begin
         Bindings.Add_Binding (Loc, LHS, RHS);
      end;

      declare
         Constraints : constant Leander.Core.Predicates.Predicate_Array :=
                         [for Tv of Tyvars =>
                            Leander.Core.Predicates.Predicate
                              ("Eq",
                               Leander.Core.Types.TVar (Tv))];
      begin
         Context.Environment.Type_Instance
           (Class_Id      => Leander.Core.To_Conid ("Eq"),
            Constraints   => Constraints,
            Instance_Type => DT.Applied_Type,
            Bindings      => Bindings.To_Core);
      end;
   end Derive_Eq;

end Leander.Syntax.Deriving;
