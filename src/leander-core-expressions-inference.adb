with Ada.Exceptions;
--  with Ada.Text_IO;
with Leander.Core.Binding_Groups.Inference;
with Leander.Core.Schemes;
with Leander.Core.Substitutions;
with Leander.Core.Type_Env;
with Leander.Core.Types.Unification;
with Leander.Logging;

package body Leander.Core.Expressions.Inference is

   -----------
   -- Infer --
   -----------

   procedure Infer
     (Context    : in out Leander.Core.Inference.Inference_Context'Class;
      Expression : Reference)
   is
      procedure Bind
        (Expr : Reference;
         Ty   : Core.Types.Reference);

      function Binding
        (Expr : Reference)
         return Core.Types.Reference;

      function TI
        (E   : Reference)
         return Leander.Core.Substitutions.Instance'Class;

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Expr : Reference;
         Ty   : Core.Types.Reference)
      is
      begin
         Context.Bind (Expr, Ty);
         --  Result.Expr_Types.Insert (Expr, Nullable_Type_Reference (Ty));
      end Bind;

      -------------
      -- Binding --
      -------------

      function Binding
        (Expr : Reference)
         return Core.Types.Reference
      is
      begin
         return Context.Binding (Expr);
      exception
         when others =>
            Context.Error ("binding not found: " & Expr.Show);
            raise;
      end Binding;

      --------
      -- TI --
      --------

      function TI
        (E   : Reference)
         return Leander.Core.Substitutions.Instance'Class
      is
      begin
         Leander.Logging.Log
           ("INFER",
            E.Show);

         case E.Tag is
            when EVar =>
               declare
                  use Leander.Core.Type_Env;
                  Sigma : constant Nullable_Scheme_Reference :=
                            Context.Type_Env.Lookup
                              (Leander.Names.Leander_Name (E.Var_Id));
               begin
                  if Sigma = null then
                     Context.Error
                       ("unbound variable: "
                        & To_String (E.Var_Id));
                     return Substitutions.Empty;
                  else
                     Bind (E, Sigma.Fresh_Instance);
                     return Substitutions.Empty;
                  end if;
               end;
            when ECon =>
               declare
                  use Leander.Core.Type_Env;
                  Sigma : constant Nullable_Scheme_Reference :=
                            Context.Type_Env.Lookup
                              (Leander.Names.Leander_Name (E.Con_Id));
               begin
                  if Sigma = null then
                     Context.Error
                       ("unbound constructor: "
                        & To_String (E.Con_Id));
                     return Substitutions.Empty;
                  else
                     Bind (E, Sigma.Fresh_Instance);
                     return Substitutions.Empty;
                  end if;
               end;
            when ELit =>
               Bind (E, E.Literal.Get_Type);
               return Substitutions.Empty;
            when EApp =>
               declare
                  Tv : constant Core.Types.Reference :=
                         Core.Types.New_TVar;
                  S1 : constant Core.Substitutions.Instance'Class :=
                         TI (E.Left);
               begin
                  Context.Save_Type_Env (Context.Type_Env.Apply (S1));

                  declare
                     S2 : constant Core.Substitutions.Instance'Class :=
                            TI (E.Right);
                     S3 : constant Core.Substitutions.Instance'Class :=
                            Core.Types.Unification.Most_General_Unifier
                              (Binding (E.Left).Apply (S2),
                               Core.Types.Fn
                                 (Binding (E.Right), Tv));
                  begin
                     Context.Restore_Type_Env;
                     if Context.OK then
                        Bind (E, Tv.Apply (S3));
                        return S3.Compose (S2).Compose (S1);
                     else
                        Context.Add_Error_Context
                          ("in " & E.Show);
                        return Substitutions.Empty;
                     end if;
                  end;
               end;
            when ELam =>
               declare
                  Tv : constant Core.Types.Reference :=
                         Core.Types.New_TVar;
               begin
                  Context.Save_Type_Env
                    (Context.Type_Env.Compose
                       (E.LVar, Schemes.To_Scheme (Tv)));
                  declare
                     S1   : constant Core.Substitutions.Instance'Class :=
                              TI (E.LBody);
                  begin
                     Context.Restore_Type_Env;
                     if Context.OK then
                        Bind (E, Types.Fn (Tv.Apply (S1), Binding (E.LBody)));
                        return S1;
                     else
                        Context.Add_Error_Context
                          ("in " & E.Show);
                        return Substitutions.Empty;
                     end if;
                  end;
               end;
            when ELet =>
               Context.Save_Type_Env;
               Core.Binding_Groups.Inference.Infer
                 (Context, E.Let_Bindings);
               Infer (Context, E.Let_Body);
               Context.Restore_Type_Env;
               Bind (E, Binding (E.Let_Body));

            return Substitutions.Empty;
         end case;

      exception
         when Ex : others =>
            Context.Error (Ada.Exceptions.Exception_Message (Ex));
            Context.Add_Error_Context
              ("in " & E.Show);
            return Substitutions.Empty;
      end TI;

      S : constant Leander.Core.Substitutions.Instance'Class :=
            TI (Expression);
   begin
      if Context.OK then
         Context.Save_Substitution (S);
         Context.Set_Result
           (Context.Binding (Expression).Apply (S));
      end if;
   end Infer;

end Leander.Core.Expressions.Inference;
