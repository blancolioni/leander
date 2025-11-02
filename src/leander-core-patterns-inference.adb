with Leander.Core.Schemes;
with Leander.Core.Type_Env;
with Leander.Core.Types;
with Leander.Core.Types.Unification;
with Leander.Logging;
package body Leander.Core.Patterns.Inference is

   procedure Infer
     (Context : in out Leander.Core.Inference.Inference_Context'Class;
      Pattern : Reference)
   is
   begin
      case Pattern.Tag is
         when PVar =>
            declare
               T : constant Leander.Core.Types.Reference :=
                     Leander.Core.Types.New_TVar;
            begin
               Context.Bind (Pattern, T);
               if To_String (Pattern.Var_Id) /= "" then
                  Context.Update_Type_Env
                    (Context.Type_Env.Compose
                       (Pattern.Var_Id,
                        Leander.Core.Schemes.To_Scheme (T)));
               end if;
            end;
         when PLit =>
            Context.Bind (Pattern, Pattern.Literal.Get_Type);
         when PCon =>
            declare
               use Leander.Core.Type_Env;
               Ts : constant Types.Type_Array (1 .. Pattern.Arg_Count) :=
                      [others => Types.New_TVar];
               Sigma : constant Nullable_Scheme_Reference :=
                         Context.Type_Env.Lookup
                           (Leander.Names.Leander_Name (Pattern.Con_Id));
            begin
               if Sigma = null then
                  Context.Error
                    ("unbound constructor: "
                     & To_String (Pattern.Con_Id));
                  return;
               end if;

               declare
                  T : Core.Types.Reference := Core.Types.New_TVar;
                  R : constant Core.Types.Reference := T;
               begin
                  for Arg_Type of reverse Ts loop
                     T := Core.Types.Fn (Arg_Type, T);
                  end loop;
                  Context.Save_Substitution
                    (Core.Types.Unification.Most_General_Unifier
                       (T, Sigma.Fresh_Instance));
                  Leander.Logging.Log
                    ("PATS",
                     Pattern.Show
                     & " ==> "
                     & Context.Current_Substitution.Show);
                  for I in Ts'Range loop
                     Context.Update_Type_Env
                       (Context.Type_Env.Compose
                          (Pattern.Con_Args (I),
                           Leander.Core.Schemes.To_Scheme (Ts (I))));
                  end loop;
                  Context.Bind (Pattern, R);
               end;
            end;
      end case;
   end Infer;

end Leander.Core.Patterns.Inference;
