with Leander.Source;

package Leander.Syntax is

   type Instance is abstract tagged private;
   type Reference is access constant Instance'Class;

private

   type Instance is abstract tagged
      record
         Location : Leander.Source.Source_Location;
      end record;

end Leander.Syntax;
