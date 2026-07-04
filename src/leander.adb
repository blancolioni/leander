with Ada.Unchecked_Deallocation;
with Leander.Handles;
with Skit.Primitives;
with Skit.Stacks;

package body Leander is

   type Binding_Instance (Argument_Count : Natural;
                          Result_Count   : Positive)
   is new Skit.Primitives.Abstraction with
      record
         Handle    : Leander.Handle;
         Eval      : Evaluator;
         Arg_Types : Foreign_Type_Array (1 .. Argument_Count);
         Res_Types : Foreign_Type_Array (1 .. Result_Count);
      end record;

   overriding function Argument_Count
     (This : Binding_Instance)
      return Natural
   is (This.Argument_Count);

   overriding procedure Evaluate
     (This  : Binding_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class);

   function Current_Environment
     (This : Handle)
      return String
   is (This.H.Current_Environment);

   function Evaluate
     (This       : Handle;
      Expression : String)
      return String
   is (This.H.Evaluate (Expression));

   function Get_Slot
     (This : Handle;
      Slot : Slot_Index)
      return Boolean
   is (This.H.Get_Slot (Slot));

   function Get_Slot
     (This : Handle;
      Slot : Slot_Index)
      return String
   is (This.H.Get_Slot (Slot));

   function Get_Slot
     (This : Handle;
      Slot : Slot_Index)
      return Integer
   is (This.H.Get_Slot (Slot));

   function Infer_Type
     (This       : Handle;
      Expression : String)
      return String
   is (This.H.Infer_Type (Expression));

   function User_Data
     (This : Handle)
      return access User_Data_Interface'Class
   is (This.User_Data);

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This           : Handle;
      Name           : String;
      Argument_Types : Foreign_Type_Array;
      Result_Type    : Foreign_Type;
      Eval           : Evaluator)
   is
      Res_Types : constant Foreign_Type_Array := [Result_Type];
   begin
      This.Bind (Name, Argument_Types, Res_Types, Eval);
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This           : Handle;
      Name           : String;
      Argument_Types : Foreign_Type_Array;
      Result_Types   : Foreign_Type_Array;
      Eval           : Evaluator)
   is
   begin
      This.H.Bind
        (Name,
         Binding_Instance'
           (Argument_Count => Argument_Types'Length,
            Result_Count   => Result_Types'Length,
            Handle         => This,
            Arg_Types      => Argument_Types,
            Res_Types      => Result_Types,
            Eval           => Eval));
   end Bind;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Handle) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Leander.Handles.Instance'Class,
           Handle_Reference);
   begin
      Free (This.H);
   end Close;

   ------------
   -- Create --
   ------------

   function Create
     (Size      : Natural := 64 * 1024;
      User_Data : access User_Data_Interface'Class)
      return Handle
   is
   begin
      return Handle'
        (H         => new Leander.Handles.Instance'
           (Leander.Handles.Create (Size)),
         User_Data => User_Data_Reference (User_Data));
   end Create;

   --------------
   -- Evaluate --
   --------------

   overriding procedure Evaluate
     (This  : Binding_Instance;
      Stack   : in out Skit.Stacks.Abstraction'Class)
   is
   begin
      for I in 1 .. This.Argument_Count loop
         This.Handle.H.Send_Value
           (Slot_Index (I), This.Arg_Types (I),
            Stack.Pop);
      end loop;
      This.Eval (This.Handle);
      for I in 1 .. This.Result_Count loop
         Stack.Push
           (This.Handle.H.Receive_Value
              (Slot_Index (I)));
      end loop;
   end Evaluate;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This      : Handle;
      Statement : String)
   is
      Result : constant String :=
                 This.Evaluate ("runIO (" & Statement & ")");
   begin
      pragma Unreferenced (Result);
   end Execute;

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module
     (This : in out Handle;
      Path : String)
   is
   begin
      This.H.Load_Module (Path);
   end Load_Module;

   ------------
   -- Report --
   ------------

   procedure Report
     (This : in out Handle)
   is
   begin
      This.H.Report;
   end Report;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : Handle;
      Slot  : Slot_Index;
      Value : Boolean)
   is
   begin
      This.H.Set_Slot (Slot, Value);
   end Set_Slot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : Handle;
      Slot  : Slot_Index;
      Value : String)
   is
   begin
      This.H.Set_Slot (Slot, Value);
   end Set_Slot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : Handle;
      Slot  : Slot_Index;
      Value : Integer)
   is
   begin
      This.H.Set_Slot (Slot, Value);
   end Set_Slot;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (This    : in out Handle;
      Enabled : Boolean)
   is
   begin
      This.H.Trace (Enabled);
   end Trace;

end Leander;
