with Leander.Syntax.Expressions.Applications;
with Leander.Syntax.Expressions.Constructors;
with Leander.Syntax.Expressions.Lambdas;
with Leander.Syntax.Expressions.Lets;
with Leander.Syntax.Expressions.Literals;
with Leander.Syntax.Expressions.Variables;

package body Leander.Syntax.Expressions is

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference
   is (Applications.Application (Location, Left, Right));

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (Constructors.Constructor (Location, Name));

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (Literals.Integer_Literal (Location, Image));

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (Variables.Variable (Location, Name));

   function Lambda
     (Location    : Leander.Source.Source_Location;
      Pat         : Leander.Syntax.Patterns.Reference;
      Expr        : Reference)
      return Reference
   is (Lambdas.Lambda (Location, Pat, Expr));

   function Let
     (Location    : Leander.Source.Source_Location;
      Bindings    : Leander.Syntax.Bindings.Reference;
      Expr        : Reference)
      return Reference
   is (Lets.Let (Location, Bindings, Expr));

end Leander.Syntax.Expressions;
