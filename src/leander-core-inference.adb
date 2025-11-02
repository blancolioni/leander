with Leander.Logging;
with Leander.Names;

package body Leander.Core.Inference is

   -----------------------
   -- Add_Error_Context --
   -----------------------

   procedure Add_Error_Context
     (This    : in out Inference_Context'Class;
      Context : String)
   is
      use Leander.Names;
   begin
      if This.OK then
         This.Error (Context);
      else
         This.Error_Message :=
           Leander.Names.To_Leander_Name
             (To_String (This.Error_Message) & Character'Val (10)
              & Context);
      end if;
   end Add_Error_Context;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This  : in out Inference_Context'Class;
      Item  : not null access constant Leander.Core.Typeable.Abstraction'Class;
      To    : Leander.Core.Types.Reference)
   is
   begin
      Leander.Logging.Log
        ("BIND", Item.Show & " :: " & To.Show);
      This.Expr_Types.Insert (Item, Nullable_Type_Reference (To));
   end Bind;

   --------------------------
   -- Current_Substitution --
   --------------------------

   function Current_Substitution
     (This : Inference_Context)
      return Leander.Core.Substitutions.Instance
   is
   begin
      return This.Subst;
   end Current_Substitution;

   -----------
   -- Error --
   -----------

   procedure Error
     (This  : in out Inference_Context'Class;
      Message : String)
   is
   begin
      This.Success := False;
      This.Error_Message := Leander.Names.To_Leander_Name (Message);
   end Error;

   ----------------------
   -- Restore_Type_Env --
   ----------------------

   procedure Restore_Type_Env (This : in out Inference_Context) is
   begin
      This.Type_Env := This.Env_Stack.Last_Element;
      This.Env_Stack.Delete_Last;
   end Restore_Type_Env;

   -----------------------
   -- Save_Substitution --
   -----------------------

   procedure Save_Substitution
     (This  : in out Inference_Context;
      Subst : Leander.Core.Substitutions.Instance'Class)
   is
   begin
      This.Subst :=
        Leander.Core.Substitutions.Instance (Subst)
        .Compose (This.Subst);
   end Save_Substitution;

   -------------------
   -- Save_Type_Env --
   -------------------

   procedure Save_Type_Env (This : in out Inference_Context) is
   begin
      This.Env_Stack.Append (This.Type_Env);
   end Save_Type_Env;

   -------------------
   -- Save_Type_Env --
   -------------------

   procedure Save_Type_Env
     (This    : in out Inference_Context;
      New_Env : Leander.Core.Type_Env.Reference)
   is
   begin
      This.Save_Type_Env;
      This.Type_Env := New_Env;
   end Save_Type_Env;

   ----------------
   -- Set_Result --
   ----------------

   procedure Set_Result
     (This  : in out Inference_Context'Class;
      Ty    : Leander.Core.Types.Reference)
   is
   begin
      This.Inferred_Type := Nullable_Type_Reference (Ty);
   end Set_Result;

   ---------------------
   -- Update_Type_Env --
   ---------------------

   procedure Update_Type_Env
     (This : in out Inference_Context;
      Env  : Leander.Core.Type_Env.Reference)
   is
   begin
      This.Type_Env := Env;
   end Update_Type_Env;

end Leander.Core.inference;
