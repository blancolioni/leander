with Ada.Text_IO;
with Leander.Calculus;

with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Syntax.Expressions;

with Skit.Compiler;
with Skit.Debug;
with Skit.Impl;
with Skit.Library;
with Skit.Machine;

package body Leander.Handles is

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Handle) is
   begin
      null;
   end Close;

   ------------
   -- Create --
   ------------

   function Create
     (Size : Natural)
      return Handle
   is
      Machine  : constant Skit.Machine.Reference :=
                   Skit.Impl.Machine (Size);
      Skit_Env : constant Skit.Environment.Reference :=
                   Skit.Environment.Create
                     (Machine);
      Context  : constant Context_Reference :=
                   new Leander.Parser.Parse_Context;
      Env      : constant Leander.Environment.Reference :=
                   Context.Load_Module
                     ("./share/leander/modules/Prelude.hs");
   begin
      Skit.Library.Load_Primitives (Skit_Env);
      return Handle'
        (Skit_Env, Env, Context);
   end Create;

   -------------------------
   -- Current_Environment --
   -------------------------

   function Current_Environment
     (This : Handle)
      return String
   is
   begin
      return This.Env.Name;
   end Current_Environment;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (This       : in out Handle;
      Expression : String)
      return String
   is
      use Leander.Core.Inference;
      use Leander.Core.Expressions.Inference;
      Syntax : constant Leander.Syntax.Expressions.Reference :=
                 This.Context.Parse_Expression (Expression);
      Core   : constant Leander.Core.Expressions.Reference :=
                 Syntax.To_Core;
      Result : Inference_Context :=
                 Initial_Context (This.Env.Type_Env);
   begin
      Leander.Syntax.Prune;
      Infer (Result, Core);
      if not Result.OK then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, Result.Error_Message);
         return "";
      else
         declare
            Tree : constant Leander.Calculus.Tree :=
                     Core.To_Calculus (Result, This.Env);
         begin
            Leander.Calculus.Compile
              (Tree, This.Env, This.Skit_Env);

            Skit.Compiler.Compile (This.Skit_Env.Machine);
            This.Skit_Env.Machine.Evaluate;
            declare
               Value : constant String :=
                         Skit.Debug.Image
                           (This.Skit_Env.Machine.Top, This.Skit_Env.Machine);
            begin
               return Value;
            end;
         end;
      end if;
   end Evaluate;

   ----------------
   -- Infer_Type --
   ----------------

   function Infer_Type
     (This       : in out Handle;
      Expression : String)
      return String
   is
      use Leander.Core.Inference;
      use Leander.Core.Expressions.Inference;
      Syntax : constant Leander.Syntax.Expressions.Reference :=
                 This.Context.Parse_Expression (Expression);
      Core   : constant Leander.Core.Expressions.Reference :=
                 Syntax.To_Core;
      Result : Inference_Context :=
                 Initial_Context (This.Env.Type_Env);
   begin
      Infer (Result, Core);
      if not Result.OK then
         return Result.Error_Message;
      else
         return Result.Get_Type (Core).Generate.Show;
      end if;
   end Infer_Type;

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module
     (This : in out Handle;
      Path : String)
   is
   begin
      This.Env := This.Context.Load_Module (Path);
      declare
         Result : constant String :=
                    This.Evaluate ("runIO main");
      begin
         if Result /= "" and then Result /= "I" then
            Ada.Text_IO.Put_Line
              (Result);
         end if;
      end;
   end Load_Module;

   ------------
   -- Report --
   ------------

   procedure Report
     (This : in out Handle)
   is
   begin
      This.Skit_Env.Machine.Report;
   end Report;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (This    : in out Handle;
      Enabled : Boolean)
   is
   begin
      This.Skit_Env.Machine.Set
        ("trace-eval",
         (if Enabled then "true" else "false"));
   end Trace;

end Leander.Handles;
