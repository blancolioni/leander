with Ada.Text_IO;
with Leander.Calculus;

with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Core.Type_Classes;
with Leander.Primitives;
with Leander.Resources;
with Leander.Syntax.Expressions;

with Skit.Combinators;
with Skit.Compiler;
with Skit.Terms;

package body Leander.Handles is

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This      : in out Instance'Class;
      Name      : String;
      Evaluator : Skit.Primitive_Evaluator_Interface'Class)
   is
      X : constant Skit.Object :=
            This.Skit_Handle.Primitive
              (Evaluator);
   begin
      This.Skit_Handle.Bind (Name, X);
   end Bind;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Instance'Class) is
   begin
      null;
   end Close;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (This       : in out Instance'Class;
      Expression : String)
   is

      function Do_Compile return String;


   
      ----------------
      -- Do_Compile --
      ----------------

      function Do_Compile return String is
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
               Tree          : constant Leander.Calculus.Tree :=
                                 Core.To_Calculus (Result, This.Env);
            begin
               Ada.Text_IO.Put_Line
                 ("compiling: " & Leander.Calculus.To_String (Tree));

               declare
                  Term          : constant Skit.Terms.Term :=
                                    Leander.Calculus.Compile (Tree);
                  Compiled_Term : constant Skit.Terms.Term :=
                                    Skit.Compiler.Compile (Term);
                  Value         : constant Skit.Object :=
                                    Skit.Terms.Install
                                      (compiled_Term, This'Access,
                                       This.Skit_Env.Machine);
               begin
                  This.Skit_Env.Machine.Push (Value);
                  Skit.Terms.Reset;
                  return Skit.Debug.Image
                    (This.Skit_Env.Machine.Top, This.Skit_Env.Machine);
               end;
            end;
         end if;
      end Do_Compile;

   begin
      --  All types inferred here are transient (ADR 0001): the surviving
      --  artifact is the off-arena Calculus.Tree installed into the machine.
      Leander.Core.Types.Begin_Scratch;
      return Result : constant String := Do_Compile do
         Leander.Core.Types.End_Scratch;
      end return;
   exception
      when others =>
         Leander.Core.Types.End_Scratch;
         raise;
         declare
            Tree          : constant Leander.Calculus.Tree :=
                              Core.To_Calculus (Result, This.Env);
            Term          : constant Skit.Terms.Term :=
              Leander.Calculus.Compile (Tree);
            Compiled_Term : constant Skit.Terms.Term :=
              Skit.Compiler.Compile (Term);

            function Resolve (Name : String) return Skit.Object
            is (This.Resolve (Name));

         begin
            This.Skit_Handle.Install
              (Compiled_Term, Resolve'Access);
            Skit.Terms.Reset;
         end;
      end if;
   end Compile;

   ------------
   -- Create --
   ------------

   function Create
     (Size      : Natural;
      User_Data : access Leander.User_Data_Interface'Class)
      return Reference
   is
      Context  : constant Context_Reference :=
                   new Leander.Parser.Parse_Context;
      Prelude_Path : constant String :=
                       Leander.Resources.Resource_Path
                       & "/modules/Prelude.hs";
      Env          : constant Leander.Environment.Reference :=
                   Context.Load_Module (Prelude_Path);
      This : constant Reference :=
               new Instance'
                     (Skit_Handle => <>,
                      Env         => Env,
                      Context     => Context,
                      User_Data   => User_Data_Reference (User_Data),
                      IO          => Leander.IO.Local_IO,
                      Slots       => <>);
   begin
      This.Skit_Handle :=
        Skit.Handles.New_Handle
          (Core_Size => Size,
           User_Data => This);
      Leander.Primitives.Load_Primitives (This.Skit_Handle);
      return This;
   end Create;

   -------------------------
   -- Current_Environment --
   -------------------------

   function Current_Environment
     (This : Instance'Class)
      return String
   is
   begin
      return This.Env.Name;
   end Current_Environment;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (This       : in out Instance'Class;
      Expression : String)
   is
   begin
      This.Compile (Expression);
      This.Skit_Handle.Evaluate;
   end Evaluate;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (This : Instance'Class;
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
     (This : Instance'Class;
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
     (This : Instance'Class;
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
     (This       : in out Instance'Class;
      Expression : String)
      return String
   is

      function Do_Infer_Type return String;

      -------------------
      -- Do_Infer_Type --
      -------------------

      function Do_Infer_Type return String is
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
      end Do_Infer_Type;

   begin
      --  Transient types only; the result is a formatted string (ADR 0001).
      Leander.Core.Types.Begin_Scratch;
      return Result : constant String := Do_Infer_Type do
         Leander.Core.Types.End_Scratch;
      end return;
   exception
      when others =>
         Leander.Core.Types.End_Scratch;
         raise;
   end Infer_Type;

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module
     (This : in out Instance'Class;
      Path : String)
   is
   begin
      This.Env := This.Context.Load_Module (Path);
   end Load_Module;

   ---------
   -- Pop --
   ---------

   function Pop
     (This : Instance'Class)
      return String
   is
   begin
      return This.Skit_Handle.Image
        (This.Skit_Handle.Pop);
   end Pop;

   -------------------
   -- Receive_Value --
   -------------------

   function Receive_Value
     (This   : Instance'Class;
      Index  : Slot_Index)
      return Skit.Object
   is
      Value : constant Foreign_Value := This.Slots (Index);
      H     : constant Skit.Handles.Handle := This.Skit_Handle;
   begin
      case Value.Class is
         when Unit_Type =>
            return Skit.Combinators.I;
         when Boolean_Type =>
            if Value.Boolean_Value then
               return Skit.Combinators.K;
            else
               H.Push (Skit.Combinators.K);
               H.Push (Skit.Combinators.I);
               H.Apply;
               return H.Pop;
            end if;
         when Integer_Type =>
            return Skit.To_Object (Value.Integer_Value);
         when String_Type =>
            declare
               use Ada.Strings.Unbounded;
               S  : constant String :=
                      To_String (Value.String_Value);
            begin
               for Ch of S loop
                  H.Push (H.Lookup ("#cons"));
                  H.Push (Skit.To_Object (Character'Pos (Ch)));
                  H.Apply;
               end loop;
               H.Push (SKit.Combinators.K);
               H.Apply;
               for I in S'Range loop
                  H.Apply;
               end loop;
               return H.Pop;
            end;
      end case;
   end Receive_Value;

   ------------
   -- Report --
   ------------

   procedure Report
     (This : in out Instance'Class)
   is
   begin
      This.Skit_Handle.Report;
      Leander.Core.Report;
      Leander.Syntax.Report;
   end Report;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (This : Instance'Class;
      Name : String)
      return Skit.Object
   is
      Binding : constant Skit.Object := This.Skit_Handle.Lookup (Name);
   begin
      if Skit.Is_Undefined (Binding) then
         if not This.Env.Variable_Binding_Exists (Name) then
            raise Program_Error with
              "undefined: " & Name;
         end if;

         declare
            T             : constant Leander.Calculus.Tree :=
                              This.Env.Get_Bound_Calculus (Name);
            Term          : constant Skit.Terms.Term :=
                              Leander.Calculus.Compile (T);
            Compiled_Term : constant Skit.Terms.Term :=
                              Skit.Compiler.Compile (Term);
            function Resolve (Name : String) return Skit.Object
            is (This.Resolve (Name));

            Value         : constant Skit.Object :=
                              This.Skit_Handle.Install
                                (Compiled_Term, Resolve'Access);
         begin
            This.Skit_Handle.Bind (Name, Value);
            return Value;
         end;
      else
         return Binding;
      end if;
   end Resolve;

   ----------------
   -- Send_Value --
   ----------------

   procedure Send_Value
     (This   : in out Instance'Class;
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
            F_Value := (Boolean_Type, Value = Skit.Combinators.K);
         when Integer_Type =>
            F_Value := (Integer_Type, Skit.To_Integer (Value));
         when String_Type =>
            declare
               use Ada.Strings.Unbounded;
               It : Skit.Object := Value;
               S  : Unbounded_String;
            begin
               while It /= Skit.Combinators.K loop
                  declare
                     Code : constant Skit.Object :=
                              This.Skit_Handle.Right
                                (This.Skit_Handle.Left (It));
                  begin
                     Append (S, Character'Val (Skit.To_Integer (Code)));
                     It := This.Skit_Handle.Right (It);
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
     (This  : in out Instance'Class;
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
     (This  : in out Instance'Class;
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
     (This  : in out Instance'Class;
      Slot  : Slot_Index;
      Value : Integer)
   is
   begin
      This.Slots (Slot) := (Integer_Type, Value);
   end Set_Slot;

end Leander.Handles;
