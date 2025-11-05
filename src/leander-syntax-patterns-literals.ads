private with Ada.Strings.Unbounded;

private package Leander.Syntax.Patterns.Literals is

   subtype Parent is Leander.Syntax.Patterns.Instance;
   type Instance is new Parent with private;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

private

   type Instance is new Parent with
      record
         Class : Syntax.Literal_Class;
         Image : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Is_Literal
     (This : Instance)
      return Boolean
   is (True);

   overriding function Literal_Class
     (This : Instance)
      return Syntax.Literal_Class
   is (This.Class);

   overriding function Literal
     (This : Instance)
      return String
   is (Ada.Strings.Unbounded.To_String (This.Image));

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Patterns.Reference;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (new Instance'(Location, Integer_Literal,
       Ada.Strings.Unbounded.To_Unbounded_String (Image)));

end Leander.Syntax.Patterns.Literals;
