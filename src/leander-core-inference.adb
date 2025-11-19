with Leander.Logging;
with Leander.Names;
with Leander.Traverseable;

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

   ------------------------
   -- Current_Predicates --
   ------------------------

   function Current_Predicates
     (This : Inference_Context)
      return Leander.Core.Predicates.Predicate_Array
   is
   begin
      return [for P of This.Predicates => P];
   end Current_Predicates;

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

   ---------------------
   -- Save_Predicates --
   ---------------------

   procedure Save_Predicates
     (This       : in out Inference_Context;
      Predicates : Leander.Core.Predicates.Predicate_Array)
   is
   begin
      for P of Predicates loop
         This.Predicates.Append (P);
      end loop;
   end Save_Predicates;

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

   -----------------
   -- Update_Type --
   -----------------

   procedure Update_Type
     (This  : Inference_Context;
      Root  : not null access
        Leander.Core.Qualified_Types.Has_Qualified_Type'Class)
   is
      procedure Update
        (Traversable : not null access Leander.Traverseable.Abstraction'Class);

      ------------
      -- Update --
      ------------

      procedure Update
        (Traversable : not null access Leander.Traverseable.Abstraction'Class)
      is
         use Leander.Core.Qualified_Types;
         HQT : Has_Qualified_Type'Class renames
                 Has_Qualified_Type'Class (Traversable.all);
      begin
         if HQT.Has_Qualified_Type_Value then
            declare
               QT  : constant Leander.Core.Qualified_Types.Reference :=
                       HQT.Qualified_Type.Apply (This.Current_Substitution);
            begin
               HQT.Set_Qualified_Type (QT);
               Leander.Logging.Log
                 ("UPDATE",
                  HQT.Show & " :: " & QT.Show);
            end;
         end if;
      end Update;

   begin
      Root.Update_Traverse (Update'Access);
   end Update_Type;

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
