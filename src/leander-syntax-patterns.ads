with Leander.Core.Patterns;

package Leander.Syntax.Patterns is

   subtype Parent is Leander.Syntax.Instance;
   type Instance is abstract new Parent with private;
   type Reference is access constant Instance'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function Is_Variable (This : Instance) return Boolean;
   function Variable_Name (This : Instance) return String;

   function Head (This : not null access constant Instance)
                  return Reference;

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   function Wildcard
     (Location : Leander.Source.Source_Location)
      return Reference;

   function Constructor
     (Location  : Leander.Source.Source_Location;
      Name      : String;
      Arguments : Reference_Array)
      return Reference;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   function To_Core (This : Instance) return Leander.Core.Patterns.Reference
                     is abstract;

private

   type Instance is abstract new Parent with
      record
         null;
      end record;

   function Is_Variable (This : Instance) return Boolean is (False);
   function Variable_Name (This : Instance) return String is ("");

end Leander.Syntax.Patterns;
