with Leander.Syntax.Types.Applications;
with Leander.Syntax.Types.Constructors;
with Leander.Syntax.Types.Variables;

package body Leander.Syntax.Types is

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

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (Variables.Variable (Location, Name));

end Leander.Syntax.Types;
