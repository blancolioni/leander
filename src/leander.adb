with Ada.Unchecked_Deallocation;
with Leander.Handles;

package body Leander is

   function Current_Environment
     (This : Handle'Class)
      return String
   is (This.H.Current_Environment);

   function Get_Slot
     (This : Handle'Class;
      Slot : Slot_Index)
      return Boolean
   is (This.H.Get_Slot (Slot));

   function Get_Slot
     (This : Handle'Class;
      Slot : Slot_Index)
      return String
   is (This.H.Get_Slot (Slot));

   function Get_Slot
     (This : Handle'Class;
      Slot : Slot_Index)
      return Integer
   is (This.H.Get_Slot (Slot));

   function Infer_Type
     (This       : Handle'Class;
      Expression : String)
      return String
   is (This.H.Infer_Type (Expression));

   function User_Data
     (This : Handle'Class)
      return access User_Data_Interface'Class
   is (This.H.User_Data);

   --------------------
   -- Argument_Modes --
   --------------------

   overriding function Argument_Modes
     (This : Binding_Instance)
      return Skit.Argument_Mode_Array
   is
   begin
      return Modes : constant Skit.Argument_Mode_Array
        (1 .. This.Argument_Count)
        := [others => Skit.Strict];
   end Argument_Modes;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This           : Handle'Class;
      Name           : String;
      Argument_Types : Foreign_Type_Array;
      Result_Type    : Foreign_Type;
      Eval           : Foreign_Function_Evaluator)
   is
      Res_Types : constant Foreign_Type_Array := [Result_Type];
   begin
      This.Bind (Name, Argument_Types, Res_Types, Eval);
   end Bind;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This           : Handle'Class;
      Name           : String;
      Argument_Types : Foreign_Type_Array;
      Result_Types   : Foreign_Type_Array;
      Eval           : Foreign_Function_Evaluator)
   is
   begin
      This.H.Bind
        (Name,
         Binding_Instance'
           (Argument_Count => Argument_Types'Length,
            Result_Count   => Result_Types'Length,
            Arg_Types      => Argument_Types,
            Res_Types      => Result_Types,
            Eval           => Eval));
   end Bind;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Handle'Class) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Leander.Handles.Instance'Class,
           Handle_Reference);
   begin
      Free (This.H);
   end Close;

   -------------
   -- Compile --
   -------------

   function Compile
     (This       : Handle'Class;
      Expression : String)
      return String
   is
   begin
      This.H.Compile (Expression);
      return This.H.Pop;
   end Compile;

   ------------
   -- Create --
   ------------

   function Create
     (Size      : Natural := 64 * 1024;
      User_Data : access User_Data_Interface'Class := null)
      return Handle
   is
   begin
      return Handle'
        (H => Handle_Reference (Leander.Handles.Create (Size, User_Data)));
   end Create;

   --------------
   -- Evaluate --
   --------------

   function Evaluate
     (This       : Handle'Class;
      Expression : String)
      return String
   is
   begin
      This.H.Evaluate (Expression);
      return This.H.Pop;
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Binding_Instance;
      User_Data : access Skit.User_Data_Interface'Class;
      Arguments : Skit.Object_Array)
      return Skit.Object
   is
      H : constant Handle_Reference := Handle_Reference (User_Data);
   begin
      for I in 1 .. This.Argument_Count loop
         H.Send_Value
           (Slot_Index (I), This.Arg_Types (I), Arguments (I));
      end loop;
      This.Eval (Handle'(H => H));
      return H.Receive_Value (1);
   end Evaluate;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (This      : Handle'Class;
      Statement : String)
   is
   begin
      This.H.Evaluate ("runIO (" & Statement & ")");
   end Execute;

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module
     (This : Handle'Class;
      Path : String)
   is
   begin
      This.H.Load_Module (Path);
   end Load_Module;

   ------------
   -- Report --
   ------------

   procedure Report
     (This : Handle'Class)
   is
   begin
      This.H.Report;
   end Report;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : Handle'Class;
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
     (This  : Handle'Class;
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
     (This  : Handle'Class;
      Slot  : Slot_Index;
      Value : Integer)
   is
   begin
      This.H.Set_Slot (Slot, Value);
   end Set_Slot;

end Leander;
