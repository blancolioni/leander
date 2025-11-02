with Leander.Syntax.Patterns.Constructors;
with Leander.Syntax.Patterns.Literals;
with Leander.Syntax.Patterns.Variables;

package body Leander.Syntax.Patterns is

   function Constructor
     (Location  : Leander.Source.Source_Location;
      Name      : String;
      Arguments : Reference_Array)
      return Reference
   is (Constructors.Constructor (Location, Name, Arguments));

   function Head (This : not null access constant Instance)
                  return Reference
   is (Reference (This));

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

   function Wildcard
     (Location : Leander.Source.Source_Location)
      return Reference
   is (Variables.Variable (Location, "_"));

end Leander.Syntax.Patterns;
