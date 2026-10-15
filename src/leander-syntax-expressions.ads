with Leander.Core.Expressions;
with Leander.Environment;
with Leander.Names;
with Leander.Syntax.Bindings;
with Leander.Syntax.Patterns;

package Leander.Syntax.Expressions is

   subtype Parent is Leander.Syntax.Instance;
   type Instance is abstract new Parent with private;
   type Reference is access constant Instance'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
      is abstract;

   function To_Pattern
     (This : Instance)
      return Leander.Syntax.Patterns.Reference;

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   function Float_Literal
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

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference;

   function Lambda
     (Location    : Leander.Source.Source_Location;
      Pat         : Leander.Syntax.Patterns.Reference;
      Expr        : not null access constant Instance'Class)
      return Reference;

   function Let
     (Location    : Leander.Source.Source_Location;
      Bindings    : Leander.Syntax.Bindings.Reference;
      Expr        : Reference)
      return Reference;

   function Head_Variable (This : not null Reference) return String;
   --  The variable at the head of This, a chain of applications or none:
   --  "f" for "f x y" and "<+>" for "a <+> b". Empty when the head is not
   --  a variable.

   function Arguments (This : not null Reference) return Reference_Array;
   --  The arguments This applies its head to, in order: "x" and "y" for
   --  "f x y". Empty when This is not an application.

   function Constructor_Name (This : not null Reference) return String;
   --  The constructor This is, when it is one on its own; otherwise empty.

   --  Field labels (Haskell 2010, section 3.15; #33).  Both forms are
   --  turned into positional ones only when they are made core, by when
   --  every data type in the module has been declared: Env is the
   --  module's environment, asked then.

   type Field_Expression is
      record
         Label : Leander.Names.Leander_Name;
         Value : Reference;
      end record;

   type Field_Expression_Array is
     array (Positive range <>) of Field_Expression;

   function Record_Construction
     (Location    : Leander.Source.Source_Location;
      Constructor : String;
      Fields      : Field_Expression_Array;
      Env         : Leander.Environment.Reference)
      return Reference;
   --  C { f1 = e1, ... }: C applied to its fields in their positions, a
   --  missing one being an error when it is used.  As a pattern, it is a
   --  record pattern.

   function Record_Update
     (Location : Leander.Source.Source_Location;
      Value    : Reference;
      Fields   : Field_Expression_Array;
      Env      : Leander.Environment.Reference)
      return Reference;
   --  e { f1 = e1, ... }: e with those fields replaced, by whichever of its
   --  type's constructors it was built with.

private

   type Instance is abstract new Parent with
      record
         null;
      end record;

   function To_Pattern
     (This : Instance)
      return Leander.Syntax.Patterns.Reference
   is (raise Constraint_Error with "invalid pattern");

end Leander.Syntax.Expressions;
