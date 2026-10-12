with Leander.Syntax.Expressions.Applications;
with Leander.Syntax.Expressions.Constructors;
with Leander.Syntax.Expressions.Lambdas;
with Leander.Syntax.Expressions.Lets;
with Leander.Syntax.Expressions.Literals;
with Leander.Syntax.Expressions.Variables;

package body Leander.Syntax.Expressions is

   ---------------
   -- Arguments --
   ---------------

   function Arguments (This : not null Reference) return Reference_Array is
   begin
      if This.all in Applications.Instance'Class then
         declare
            App : Applications.Instance'Class renames
                    Applications.Instance'Class (This.all);
         begin
            return Arguments (App.Function_Part) & App.Argument_Part;
         end;
      else
         return [];
      end if;
   end Arguments;

   -------------------
   -- Head_Variable --
   -------------------

   function Head_Variable (This : not null Reference) return String is
   begin
      if This.all in Applications.Instance'Class then
         return Head_Variable
           (Applications.Instance'Class (This.all).Function_Part);
      elsif This.all in Variables.Instance'Class then
         return Variables.Instance'Class (This.all).Variable_Name;
      else
         return "";
      end if;
   end Head_Variable;

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

   function Float_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (Literals.Float_Literal (Location, Image));

   function Character_Literal
     (Location : Leander.Source.Source_Location;
      Index    : Natural)
      return Reference
   is (Literals.Character_Literal (Location, Index));

   function String_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (Literals.String_Literal (Location, Image));

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (Variables.Variable (Location, Name));

   function Lambda
     (Location    : Leander.Source.Source_Location;
      Pat         : Leander.Syntax.Patterns.Reference;
      Expr        : not null access constant Instance'Class)
      return Reference
   is (Lambdas.Lambda (Location, Pat, Reference (Expr)));

   function Let
     (Location    : Leander.Source.Source_Location;
      Bindings    : Leander.Syntax.Bindings.Reference;
      Expr        : Reference)
      return Reference
   is (Lets.Let (Location, Bindings, Expr));

end Leander.Syntax.Expressions;
