private with Ada.Strings.Unbounded;
private with Leander.Environment;
with Leander.Parser;
private with Skit.Environment;
with Skit.Primitives;
with Skit.Terms;

package Leander.Handles is

   type Instance is new Skit.Terms.Resolver_Interface with private;

   function Create
     (Size : Natural)
      return Instance;

   function Current_Environment
     (This : Instance)
      return String;

   function Evaluate
     (This       : in out Instance;
      Expression : String)
      return String;

   function Compile
     (This       : in out Instance;
      Expression : String)
      return String;

   function Infer_Type
     (This       : in out Instance;
      Expression : String)
      return String;

   procedure Load_Module
     (This : in out Instance;
      Path : String);

   procedure Report
     (This : in out Instance);

   procedure Trace
     (This    : in out Instance;
      Enabled : Boolean);

   procedure Close (This : in out Instance);

   procedure Send_Value
     (This   : in out Instance;
      Index  : Slot_Index;
      F_Type : Foreign_Type;
      Value  : Skit.Object);

   function Receive_Value
     (This   : Instance;
      Index  : Slot_Index)
      return Skit.Object;

   procedure Set_Slot
     (This  : in out Instance;
      Slot  : Slot_Index;
      Value : Boolean);

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return Boolean;

   procedure Set_Slot
     (This  : in out Instance;
      Slot  : Slot_Index;
      Value : String);

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return String;

   procedure Set_Slot
     (This  : in out Instance;
      Slot  : Slot_Index;
      Value : Integer);

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return Integer;

   procedure Bind
     (This      : Instance;
      Name      : String;
      Primitive : Skit.Primitives.Abstraction'Class);

private

   type Context_Reference is access all Leander.Parser.Parse_Context;

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

   type Instance is new Skit.Terms.Resolver_Interface with
      record
         Skit_Env : Skit.Environment.Reference;
         Env      : Leander.Environment.Reference;
         Context  : Context_Reference;
         Slots    : Foreign_Slots;
      end record;

   overriding function Resolve
     (This : Instance;
      Name : String)
      return Skit.Object;

end Leander.Handles;
