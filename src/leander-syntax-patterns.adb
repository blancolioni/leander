with Leander.Syntax.Patterns.Variables;

package body Leander.Syntax.Patterns is

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference
   is (null);

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (null);

   function Head (This : not null access constant Instance)
                  return Reference
   is (Reference (This));

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (Variables.Variable (Location, Name));

end Leander.Syntax.Patterns;
