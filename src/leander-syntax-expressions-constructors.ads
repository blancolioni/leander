with Leander.Core.Types;

private package Leander.Syntax.Expressions.Constructors is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

   function Conid (This : Instance'Class) return Core.Conid;

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference;

private

   type Instance is new Parent with
      record
         Name     : Leander.Core.Conid;
         Con_Type : Leander.Core.Types.Reference;
      end record;

   overriding function To_Pattern
     (This : Instance)
      return Leander.Syntax.Patterns.Reference
   is (Leander.Syntax.Patterns.Constructor
       (This.Location, Leander.Core.To_String (This.Name),
          []));

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is (Leander.Core.Expressions.Constructor
       (This.Location, This.Name));

   function Conid (This : Instance'Class) return Core.Conid
   is (This.Name);

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Syntax.Allocate (This)));

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (Allocate (Instance'(Location, Leander.Core.To_Conid (Name),
       Core.Types.T_Error)));

end Leander.Syntax.Expressions.Constructors;
