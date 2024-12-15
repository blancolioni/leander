with Leander.Source;

package Leander.Syntax is

   type Instance is abstract tagged private;
   type Reference is access constant Instance'Class;

   function Location
     (This : Instance'Class)
      return Leander.Source.Source_Location;

private

   type Instance is abstract tagged
      record
         Location : Leander.Source.Source_Location;
      end record;

   function Location
     (This : Instance'Class)
      return Leander.Source.Source_Location
   is (This.Location);

end Leander.Syntax;
