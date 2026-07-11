private with Ada.Strings.Unbounded;
private with Leander.Environment;
with Leander.IO;
with Leander.Parser;
with Skit;
private with Skit.Handles;

package Leander.Handles is

   type Instance is new Skit.User_Data_Interface with private;
   type Reference is access all Instance'Class;

   function Create
     (Size      : Natural;
      User_Data : access Leander.User_Data_Interface'Class)
      return Reference;

   function Current_Environment
     (This : Instance'Class)
      return String;

   procedure Evaluate
     (This       : in out Instance'Class;
      Expression : String);

   procedure Compile
     (This       : in out Instance'Class;
      Expression : String);

   function Pop
     (This : Instance'Class)
      return String;

   function Infer_Type
     (This       : in out Instance'Class;
      Expression : String)
      return String;

   procedure Load_Module
     (This : in out Instance'Class;
      Path : String);

   function User_Data
     (This : Instance'Class)
      return access User_Data_Interface'Class;

   function IO
     (This : Instance'Class)
      return Leander.IO.Reference;

   procedure Report
     (This : in out Instance'Class);

   procedure Close (This : in out Instance'Class);

   procedure Set_Slot
     (This  : in out Instance'Class;
      Slot  : Slot_Index;
      Value : Boolean);

   function Get_Slot
     (This : Instance'Class;
      Slot : Slot_Index)
      return Boolean;

   procedure Set_Slot
     (This  : in out Instance'Class;
      Slot  : Slot_Index;
      Value : String);

   function Get_Slot
     (This : Instance'Class;
      Slot : Slot_Index)
      return String;

   procedure Set_Slot
     (This  : in out Instance'Class;
      Slot  : Slot_Index;
      Value : Integer);

   function Get_Slot
     (This : Instance'Class;
      Slot : Slot_Index)
      return Integer;

   procedure Bind
     (This      : in out Instance'Class;
      Name      : String;
      Evaluator : Skit.Primitive_Evaluator_Interface'Class);

   procedure Send_Value
     (This   : in out Instance'Class;
      Index  : Slot_Index;
      F_Type : Foreign_Type;
      Value  : Skit.Object);

   function Receive_Value
     (This   : Instance'Class;
      Index  : Slot_Index)
      return Skit.Object;

   function Debug_Image
     (This : Instance'Class;
      Value : Skit.Object)
      return String;

private

   type Context_Reference is access all Leander.Parser.Parse_Context;
   type User_Data_Reference is access all Leander.User_Data_Interface'Class;

   type Foreign_Value (Class : Foreign_Type_Class := Unit_Type) is
      record
         case Class is
            when Unit_Type =>
               null;
            when Boolean_Type =>
               Boolean_Value : Boolean;
            when Integer_Type =>
               Integer_Value : Integer;
            when String_Type =>
               String_Value  : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   type Foreign_Slots is array (Slot_Index) of Foreign_Value;

   type Instance is new Skit.User_Data_Interface with
      record
         Skit_Handle : Skit.Handles.Handle;
         Env         : Leander.Environment.Reference;
         Context     : Context_Reference;
         User_Data   : User_Data_Reference;
         Slots       : Foreign_Slots;
         IO          : Leander.IO.Reference;
      end record;

   function User_Data
     (This : Instance'Class)
      return access User_Data_Interface'Class
   is (This.User_Data);

   function Resolve
     (This : Instance'Class;
      Name : String)
      return Skit.Object;

   function IO
     (This : Instance'Class)
      return Leander.IO.Reference
   is (This.IO);

   function Debug_Image
     (This : Instance'Class;
      Value : Skit.Object)
      return String
   is (This.Skit_Handle.Image (Value));

end Leander.Handles;
