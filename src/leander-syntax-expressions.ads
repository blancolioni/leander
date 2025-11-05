with Leander.Core.Expressions;
with Leander.Syntax.Bindings;
with Leander.Syntax.Patterns;

package Leander.Syntax.Expressions is

   subtype Parent is Leander.Syntax.Instance;
   type Instance is abstract new Parent with private;
   type Reference is access constant Instance'Class;

   function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
      is abstract;

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

private

   type Instance is abstract new Parent with
      record
         null;
      end record;

end Leander.Syntax.Expressions;
