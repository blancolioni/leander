with Leander.Core;

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
         Name : Leander.Core.Name_Id;
      end record;

   overriding function Is_Variable (This : Instance) return Boolean is (True);
   overriding function Variable_Name (This : Instance) return String
   is (Leander.Core.Show (This.Name));

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (new Instance'(Location, Leander.Core.Id (Name)));

end Leander.Syntax.Patterns.Variables;
