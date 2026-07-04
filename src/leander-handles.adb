with Ada.Exceptions;
with Ada.Text_IO;
with Leander.Calculus;

with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Core.Type_Classes;
with Leander.Syntax.Expressions;

with Skit.Compiler;
with Skit.Debug;
with Skit.Impl;
with Skit.Library;
with Skit.Machine;

package body Leander.Handles is

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This      : Instance;
      Name      : String;
      Primitive : Skit.Primitives.Abstraction'Class)
   is
      X : constant Skit.Object :=
            This.Skit_Env.Machine.Bind
              (Primitive);
   begin
      This.Skit_Env.Bind (Name, X);
   end Bind;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Instance) is
   begin
      null;
   end Close;

   -------------
   -- Compile --
   -------------

   function Compile
     (This       : in out Instance;
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

         Result.Update_Type (Core);

         declare
            Tree : constant Leander.Calculus.Tree :=
                     Core.To_Calculus (Result, This.Env);
         begin
            Leander.Calculus.Compile
              (Tree, This.Env, This.Skit_Env);
            Skit.Compiler.Compile (This.Skit_Env.Machine);
            return Skit.Debug.Image
                     (This.Skit_Env.Machine.Top, This.Skit_Env.Machine);
         end;
      end if;
   end Compile;

   ------------
   -- Create --
   ------------

   function Create
     (Size : Natural)
      return Instance
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
      return Instance'
        (Skit_Env, Env, Context, others => <>);
   end Create;

   -------------------------
   -- Current_Environment --
   -------------------------

   function Current_Environment
     (This : Instance)
      return String
   is
   begin
      return This.Env.Name;
   end Current_Environment;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (This       : in out Instance;
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
      begin
         Infer (Result, Core);
      exception
         when E : others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "Unhandled: " & Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               Ada.Exceptions.Exception_Information (E));
            if not Result.OK then
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  "possibly triggered by the following");
               Ada.Text_IO.Put_Line
                 (Ada.Text_IO.Standard_Error,
                  Result.Error_Message);
            end if;
            return "";
      end;

      if not Result.OK then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, Result.Error_Message);
         return "";
      else

         Result.Update_Type (Core);

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

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return Boolean
   is
   begin
      return This.Slots (Slot).Boolean_Value;
   end Get_Slot;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return String
   is
   begin
      return Ada.Strings.Unbounded.to_string
        (This.Slots (Slot).String_Value);
   end Get_Slot;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return Integer
   is
   begin
      return This.Slots (Slot).Integer_Value;
   end Get_Slot;

   ----------------
   -- Infer_Type --
   ----------------

   function Infer_Type
     (This       : in out Instance;
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
         declare
            Ps      : constant Leander.Core.Predicates.Predicate_Array :=
                        Result.Current_Predicates;
            Success : Boolean;
            Reduced : constant Leander.Core.Predicates.Predicate_Array :=
                        Leander.Core.Type_Classes.Class_Environment'Class
                          (This.Env.all).Reduce (Ps, Success);
         begin
            return Leander.Core.Qualified_Types.Qualified_Type
                     ((if Success then Reduced else Ps),
                      Result.Get_Type (Core)).Generate.Show;
         end;
      end if;
   end Infer_Type;

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module
     (This : in out Instance;
      Path : String)
   is
   begin
      This.Env := This.Context.Load_Module (Path);
   end Load_Module;

   -------------------
   -- Receive_Value --
   -------------------

   function Receive_Value
     (This   : Instance;
      Index  : Slot_Index)
      return Skit.Object
   is
      Value : constant Foreign_Value := This.Slots (Index);
      M     : constant Skit.Machine.Reference :=
                This.Skit_Env.Machine;
   begin
      case Value.Class is
         when Unit_Type =>
            return Skit.I;
         when Boolean_Type =>
            if Value.Boolean_Value then
               return Skit.K;
            else
               M.Push (Skit.K);
               M.Push (Skit.I);
               M.Apply;
               return M.Pop;
            end if;
         when Integer_Type =>
            return Skit.To_Object (Value.Integer_Value);
         when String_Type =>
            declare
               use Ada.Strings.Unbounded;
               S  : constant String :=
                      To_String (Value.String_Value);
               E  : Unbounded_String := To_Unbounded_String ("K");
            begin
               for Ch of reverse S loop
                  E := "B* (B K) C (C I)"
                    & Natural'Image (Character'Pos (Ch))
                    & " ("
                    & E & ")";
               end loop;
               This.Skit_Env.Parse (To_String (E));
               return M.Pop;
            end;
      end case;
   end Receive_Value;

   ------------
   -- Report --
   ------------

   procedure Report
     (This : in out Instance)
   is
   begin
      This.Skit_Env.Machine.Report;
   end Report;

   ----------------
   -- Send_Value --
   ----------------

   procedure Send_Value
     (This   : in out Instance;
      Index  : Slot_Index;
      F_Type : Foreign_Type;
      Value  : Skit.Object)
   is
      use type Skit.Object;
      F_Value : Foreign_Value;
   begin
      case F_Type.Class is
         when Unit_Type =>
            F_Value := (Class => Unit_Type);
         when Boolean_Type =>
            F_Value := (Boolean_Type, Value = Skit.K);
         when Integer_Type =>
            F_Value := (Integer_Type, Skit.To_Integer (Value));
         when String_Type =>
            declare
               use Ada.Strings.Unbounded;
               It : Skit.Object := Value;
               S  : Unbounded_String;
            begin
               while It /= Skit.K loop
                  declare
                     Code : constant Skit.Object :=
                              This.Skit_Env.Machine.Right
                                (This.Skit_Env.Machine.Left
                                   (It));
                  begin
                     Ada.Text_IO.Put_Line
                       (Skit.Debug.Image (Code));
                     Append (S, Character'Val (Skit.To_Integer (Code)));
                     It := This.Skit_Env.Machine.Right (It);
                  end;
               end loop;
               F_Value := (String_Type, S);
            end;
      end case;
      This.Slots (Index) := F_Value;
   end Send_Value;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : in out Instance;
      Slot  : Slot_Index;
      Value : Boolean)
   is
   begin
      This.Slots (Slot) := (Boolean_Type, Value);
   end Set_Slot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : in out Instance;
      Slot  : Slot_Index;
      Value : String)
   is
   begin
      This.Slots (Slot) :=
        (String_Type, Ada.Strings.Unbounded.To_Unbounded_String (Value));
   end Set_Slot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : in out Instance;
      Slot  : Slot_Index;
      Value : Integer)
   is
   begin
      This.Slots (Slot) := (Integer_Type, Value);
   end Set_Slot;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (This    : in out Instance;
      Enabled : Boolean)
   is
   begin
      This.Skit_Env.Machine.Set
        ("trace-eval",
         (if Enabled then "true" else "false"));
   end Trace;

end Leander.Handles;
