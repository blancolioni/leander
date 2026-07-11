limited with Leander.Handles;

package Leander is

   type User_Data_Interface is limited interface;

   type Handle is tagged private;

   function Create
     (Size      : Natural := 64 * 1024;
      User_Data : access User_Data_Interface'Class := null)
      return Handle;

   function User_Data
     (This : Handle'Class)
      return access User_Data_Interface'Class;

   function Current_Environment
     (This : Handle'Class)
      return String;

   function Compile
     (This       : Handle'Class;
      Expression : String)
      return String;

   function Evaluate
     (This       : Handle'Class;
      Expression : String)
      return String;

   procedure Execute
     (This      : Handle'Class;
      Statement : String);

   function Infer_Type
     (This       : Handle'Class;
      Expression : String)
      return String;

   procedure Load_Module
     (This : Handle'Class;
      Path : String);

   procedure Report
     (This : Handle'Class);

   procedure Close (This : in out Handle'Class);

   type Foreign_Type is private;

   function Boolean_Type return Foreign_Type;
   function Integer_Type return Foreign_Type;
   function String_Type return Foreign_Type;

   type Foreign_Type_Array is array (Positive range <>) of Foreign_Type;

   type Slot_Index is range 1 .. 16;

   procedure Set_Slot
     (This  : Handle'Class;
      Slot  : Slot_Index;
      Value : Boolean);

   function Get_Slot
     (This : Handle'Class;
      Slot : Slot_Index)
      return Boolean;

   procedure Set_Slot
     (This  : Handle'Class;
      Slot  : Slot_Index;
      Value : String);

   function Get_Slot
     (This : Handle'Class;
      Slot : Slot_Index)
      return String;

   procedure Set_Slot
     (This  : Handle'Class;
      Slot  : Slot_Index;
      Value : Integer);

   function Get_Slot
     (This : Handle'Class;
      Slot : Slot_Index)
      return Integer;

   type Foreign_Function_Evaluator is access
     procedure (H : Handle'Class);

   procedure Bind
     (This           : Handle'Class;
      Name           : String;
      Argument_Types : Foreign_Type_Array;
      Result_Type    : Foreign_Type;
      Eval           : Foreign_Function_Evaluator);

   procedure Bind
     (This           : Handle'Class;
      Name           : String;
      Argument_Types : Foreign_Type_Array;
      Result_Types   : Foreign_Type_Array;
      Eval           : Foreign_Function_Evaluator);

private

   type Handle_Reference is access all Leander.Handles.Instance'Class;

   type Handle is tagged
      record
         H : Handle_Reference;
      end record;

   type Foreign_Type_Class is
     (Unit_Type, Boolean_Type, Integer_Type, String_Type);

   type Foreign_Type (Class : Foreign_Type_Class := Unit_Type) is
      record
         null;
      end record;

   function Boolean_Type return Foreign_Type
   is (Class => Boolean_Type);

   function Integer_Type return Foreign_Type
   is (Class => Integer_Type);

   function String_Type return Foreign_Type
   is (Class => String_Type);

end Leander;
