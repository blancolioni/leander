private package Leander.Syntax.Types.Applications is

   subtype Parent is Leander.Syntax.Types.Instance;
   type Instance is new Parent with private;

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Types.Reference;

private

   type Instance is new Parent with
      record
         Left, Right : Reference;
      end record;

   overriding function Head
     (This : not null access constant Instance)
      return Reference
   is (This.Left.Head);

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Types.Reference
   is (Leander.Core.Types.Application
         (This.Left.To_Core, This.Right.To_Core));

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Syntax.Allocate (This)));

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference
   is (Allocate (Instance'(Location, Core.Kinds.Star, Left, Right)));

end Leander.Syntax.Types.Applications;
