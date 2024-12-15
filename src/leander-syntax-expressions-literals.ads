private package Leander.Syntax.Expressions.Literals is

   subtype Parent is Leander.Syntax.Expressions.Instance;
   type Instance is new Parent with private;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Expressions.Reference;

private

   type Literal_Class is
     (Integer_Literal);

   type Instance is new Parent with
      record
         Class : Literal_Class;
         Image : Leander.Core.Name_Id;
      end record;

   function Integer_Literal
     (Location : Leander.Source.Source_Location;
      Image    : String)
      return Reference
   is (new Instance'(Location, Integer_Literal, Leander.Core.Id (Image)));

end Leander.Syntax.Expressions.Literals;
