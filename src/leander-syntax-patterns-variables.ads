with Leander.Names;

private package Leander.Syntax.Patterns.Variables is

   subtype Parent is Leander.Syntax.Patterns.Instance;
   type Instance is new Parent with private;

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

private

   type Instance is new Parent with
      record
         Name : Leander.Names.Leander_Name;
      end record;

   overriding function Is_Variable (This : Instance) return Boolean is (True);
   overriding function Variable_Name (This : Instance) return String
   is (Leander.Names.To_String (This.Name));
   overriding function To_Core
     (This : Instance)
      return Leander.Core.Patterns.Reference
   is (Leander.Core.Patterns.Variable (Core.Varid (This.Name)));

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (new Instance'(Location, Leander.Names.To_Leander_Name (Name)));

end Leander.Syntax.Patterns.Variables;
