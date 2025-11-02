private with Ada.Strings.Unbounded;

private package Leander.Syntax.Patterns.Literals is

   subtype Parent is Leander.Syntax.Patterns.Instance;
   type Instance is new Parent with private;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

private

   type Literal_Class is
     (Integer_Literal);

   type Instance is new Parent with
      record
         Class : Literal_Class;
         Image : Ada.Strings.Unbounded.Unbounded_String;
      end record;

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
