private with Ada.Strings.Unbounded;

private package Leander.Syntax.Expressions.Literals is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   function Character_Literal
     (Location : Leander.Source.Source_Location;
      Index    : Natural)
      return Reference;

   function String_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   function Float_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference;

private

   type Instance is new Parent with
      record
         Class : Literal_Class;
         Index : Natural;
         Image : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function To_Pattern
     (This : Instance)
      return Leander.Syntax.Patterns.Reference;

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Syntax.Allocate (This)));

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (Allocate (Instance'(Location, Integer_Literal, 0,
       Ada.Strings.Unbounded.To_Unbounded_String (Image))));

   function Character_Literal
     (Location : Leander.Source.Source_Location;
      Index    : Natural)
      return Reference
   is (Allocate (Instance'(Location, Character_Literal, Index,
       Ada.Strings.Unbounded.Null_Unbounded_String)));

   function String_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (Allocate (Instance'(Location, String_Literal, 0,
       Ada.Strings.Unbounded.To_Unbounded_String (Image))));

   function Float_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (Allocate (Instance'(Location, Float_Literal, 0,
       Ada.Strings.Unbounded.To_Unbounded_String (Image))));

end Leander.Syntax.Expressions.Literals;
