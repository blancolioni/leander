private with Leander.Environment;
with Leander.Parser;
private with Skit.Environment;
with Skit.Primitives;

package Leander.Handles is

   type Instance is tagged private;

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

   procedure Set_Slot
     (This   : Instance;
      Index  : Slot_Index;
      F_Type : Foreign_Type;
      Value  : Skit.Object);

   function Get_Slot
     (This   : Instance;
      Index  : Slot_Index;
      F_Type : Foreign_Type)
      return Skit.Object;

   procedure Set_Slot
     (This  : Instance;
      Slot  : Slot_Index;
      Value : Boolean);

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return Boolean;

   procedure Set_Slot
     (This  : Instance;
      Slot  : Slot_Index;
      Value : String);

   function Get_Slot
     (This : Instance;
      Slot : Slot_Index)
      return String;

   procedure Set_Slot
     (This  : Instance;
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

   type Instance is tagged
      record
         Skit_Env : Skit.Environment.Reference;
         Env      : Leander.Environment.Reference;
         Context  : Context_Reference;
      end record;

end Leander.Handles;
