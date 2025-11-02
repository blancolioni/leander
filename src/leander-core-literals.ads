private with Ada.Strings.Unbounded;

with Leander.Calculus;
with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Literals is

   type Instance is new Leander.Showable.Abstraction with private;

   function Get_Type
     (This : Instance)
      return Leander.Core.Types.Reference;

   function To_Calculus
     (This : Instance)
      return Leander.Calculus.Tree;

   function Integer_Literal
     (Image : String)
      return Instance;

   function Character_Literal
     (Code : Natural)
      return Instance;

   function Float_Literal
     (Image : String)
      return Instance;

   function String_Literal
     (S : String)
      return Instance;

private

   type Instance_Tag is (LChar, LFloat, LInteger, LString);

   type Instance is new Leander.Showable.Abstraction with
      record
         Tag   : Instance_Tag;
         Image : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Show
     (This : Instance)
      return String
   is (Ada.Strings.Unbounded.To_String (This.Image));

end Leander.Core.Literals;
