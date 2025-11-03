with Leander.Core.Expressions.Inference;
with Leander.Core.Patterns.Inference;
with Leander.Core.Types;
with Leander.Logging;

package body Leander.Core.Alts.Inference is

   -----------
   -- Infer --
   -----------

   procedure Infer
     (Context : in out Leander.Core.Inference.Inference_Context'Class;
      Alt     : Reference)
   is
   begin

      Context.Save_Type_Env;

      if Alt.Has_Pattern then
         Leander.Core.Patterns.Inference.Infer (Context, Alt.Pat);
      end if;

      Leander.Core.Expressions.Inference.Infer (Context, Alt.Expr);

      Context.Restore_Type_Env;

      if not Context.OK then
         Context.Add_Error_Context
           ("in " & Alt.Show);
         return;
      end if;

      if Alt.Has_Pattern then
         declare
            Pat_T : constant Leander.Core.Types.Reference :=
                      Context.Binding (Alt.Pat);
            T     : Leander.Core.Types.Reference :=
                      Context.Binding (Alt.Expr);
         begin
            Leander.Logging.Log
              ("ALT", Alt.Show & " :: " & T.Show);
            T := Core.Types.Fn (Pat_T, T);
            Leander.Logging.Log
              ("ALT", Alt.Show & " :: " & T.Show);
            Context.Bind (Alt, T);
         end;
      else
         Context.Bind (Alt, Context.Binding (Alt.Expr));
      end if;
   end Infer;

end Leander.Core.Alts.Inference;
