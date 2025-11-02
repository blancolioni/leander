with Leander.Core.Expressions.Inference;
with Leander.Core.Patterns.Inference;
with Leander.Core.Types;
with Leander.Logging;

package body Leander.Core.Alts.Inference is

   type Nullable_Type_Reference is
     access constant Leander.Core.Types.Instance'Class;

   type Type_Array is array (Positive range <>) of Nullable_Type_Reference;

   -----------
   -- Infer --
   -----------

   procedure Infer
     (Context : in out Leander.Core.Inference.Inference_Context'Class;
      Alt     : Reference)
   is
      Ts : Type_Array (1 .. Alt.Pat_Count);
      Last : Natural := 0;
   begin

      Context.Save_Type_Env;

      for Pat of Alt.Pats loop
         Leander.Core.Patterns.Inference.Infer (Context, Pat);
         Last := @ + 1;
         Ts (Last) := Nullable_Type_Reference (Context.Binding (Pat));
      end loop;

      Leander.Core.Expressions.Inference.Infer (Context, Alt.Expr);

      Context.Restore_Type_Env;

      if not Context.OK then
         Context.Add_Error_Context
           ("in " & Alt.Show);
         return;
      end if;

      declare
         T : Leander.Core.Types.Reference := Context.Binding (Alt.Expr);
      begin
         Leander.Logging.Log
           ("ALT", Alt.Show & " :: " & T.Show);
         for Pat_Type of reverse Ts loop
            T := Core.Types.Fn (Core.Types.Reference (Pat_Type), T);
            Leander.Logging.Log
              ("ALT", Alt.Show & " :: " & T.Show);
         end loop;
         Context.Bind (Alt, T);
      end;

   end Infer;

end Leander.Core.Alts.Inference;
