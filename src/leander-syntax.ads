with Leander.Environment;
with Leander.Source;

package Leander.Syntax is

   type Instance is abstract tagged private;
   type Reference is access all Instance'Class;

   function Location
     (This : Instance'Class)
      return Leander.Source.Source_Location;

   procedure Decorate
     (This : in out Instance;
      Env  : Leander.Environment.Reference)
   is null;

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
