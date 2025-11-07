private package Leander.Syntax.Expressions.Variables is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference;

private

   type Instance is new Parent with
      record
         Name : Leander.Core.Varid;
      end record;

   overriding function To_Pattern
     (This : Instance)
      return Leander.Syntax.Patterns.Reference
   is (Leander.Syntax.Patterns.Variable
       (This.Location,
          Leander.Core.To_String (This.Name)));

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference
   is (Leander.Core.Expressions.Variable
       (This.Location, This.Name));

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (new Instance'(Location, Leander.Core.To_Varid (Name)));

end Leander.Syntax.Expressions.Variables;
