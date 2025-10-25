with Leander.Core.Types;

private package Leander.Syntax.Expressions.Constructors is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

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

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is (Leander.Core.Expressions.Constructor (This.Name));

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (new Instance'(Location, Leander.Core.To_Conid (Name),
       Core.Types.T_Error));

end Leander.Syntax.Expressions.Constructors;
