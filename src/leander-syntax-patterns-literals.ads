private with Ada.Strings.Unbounded;

private package Leander.Syntax.Patterns.Literals is

   subtype Parent is Leander.Syntax.Patterns.Instance;
   type Instance is new Parent with private;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   function Character_Literal
     (Location : Leander.Source.Source_Location;
      Code     : Natural)
      return Reference;

   function String_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   function Float_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

private

   type Instance is new Parent with
      record
         Class : Syntax.Literal_Class;
         Code  : Natural;
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

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Syntax.Allocate (This)));

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (Allocate (Instance'
         (Location => Location,
          Class    => Integer_Literal,
          Code     => 0,
          Image    => Ada.Strings.Unbounded.To_Unbounded_String (Image))));

   function Character_Literal
     (Location : Leander.Source.Source_Location;
      Code     : Natural)
      return Reference
   is (Allocate (Instance'(Location, Character_Literal, Code,
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

end Leander.Syntax.Patterns.Literals;
