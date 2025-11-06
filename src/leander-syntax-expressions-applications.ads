private package Leander.Syntax.Expressions.Applications is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference;

private

   type Instance is new Parent with
      record
         Left, Right : Reference;
      end record;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is (Leander.Core.Expressions.Application
         (This.Location, This.Left.To_Core, This.Right.To_Core));

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference
   is (new Instance'(Location, Left, Right));

end Leander.Syntax.Expressions.Applications;
