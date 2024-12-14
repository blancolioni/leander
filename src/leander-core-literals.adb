with Ada.Strings.Unbounded;

package body Leander.Core.Literals is

   type Literal_Class is
     (Character_Literal,
      Float_Literal,
      Integer_Literal,
      String_Literal);

   type Instance (Class : Literal_Class) is new Abstraction with
      record
         Image : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Literal_Type
     (This : Instance)
      return Leander.Core.Types.Reference;

   overriding function Show
     (This : Instance)
      return String;

   function Allocate
     (Class  : Literal_Class;
      Source : String)
      return Reference;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (Class  : Literal_Class;
      Source : String)
      return Reference
   is
   begin
      return new Instance'
        (Class, Ada.Strings.Unbounded.To_Unbounded_String (Source));
   end Allocate;

   -----------------------
   -- Character_Literal --
   -----------------------

   function Character_Literal
     (Code : Natural)
      return Reference
   is
   begin
      return Allocate (Character_Literal, Code'Image);
   end Character_Literal;

   -------------------
   -- Float_Literal --
   -------------------

   function Float_Literal
     (Image : String)
      return Reference
   is
   begin
      return Allocate (Float_Literal, Image);
   end Float_Literal;

   ---------------------
   -- Integer_Literal --
   ---------------------

   function Integer_Literal
     (Image : String)
      return Reference
   is
   begin
      return Allocate (Integer_Literal, Image);
   end Integer_Literal;

   overriding function Literal_Type
     (This : Instance)
      return Leander.Core.Types.Reference
   is
   begin
      case This.Class is
         when Character_Literal =>
            return Types.T_Char;
         when Float_Literal =>
            return Types.T_Double;
         when Integer_Literal =>
            return Types.T_Int;
         when String_Literal =>
            return Types.T_String;
      end case;
   end Literal_Type;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String (This.Image);
   end Show;

   --------------------
   -- String_Literal --
   --------------------

   function String_Literal
     (S : String)
      return Reference
   is
   begin
      return Allocate (String_Literal, S);
   end String_Literal;

end Leander.Core.Literals;
