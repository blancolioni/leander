with Leander.Core.Types;

private package Leander.Syntax.Types.Constructors is

   subtype Parent is Leander.Syntax.Types.Instance;
   type Instance is new Parent with private;

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

private

   type Instance is new Parent with
      record
         Name     : Leander.Core.Conid;
      end record;

   overriding function Head
     (This : not null access constant Instance)
      return Reference
   is (Reference (This));

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Types.Reference;

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Syntax.Allocate (This)));

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (Allocate (Instance'(Location, Core.Kinds.Star,
       Leander.Core.To_Conid (Name))));

end Leander.Syntax.Types.Constructors;
