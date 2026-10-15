private package Leander.Syntax.Expressions.Records is

   --  Record construction and update (Haskell 2010, sections 3.15.2 and
   --  3.15.3; #33), turned into positional forms when they are made core.

   subtype Parent is Leander.Syntax.Expressions.Instance;

   function Construction
     (Location    : Leander.Source.Source_Location;
      Constructor : String;
      Fields      : Field_Expression_Array;
      Env         : Leander.Environment.Reference)
      return Reference;

   function Update
     (Location : Leander.Source.Source_Location;
      Value    : Reference;
      Fields   : Field_Expression_Array;
      Env      : Leander.Environment.Reference)
      return Reference;

private

   type Construction_Instance (Count : Natural) is new Parent with
      record
         Constructor : Leander.Names.Leander_Name;
         Fields      : Field_Expression_Array (1 .. Count);
         Env         : Leander.Environment.Reference;
      end record;

   overriding function To_Core
     (This : Construction_Instance)
      return Leander.Core.Expressions.Reference;

   overriding function To_Pattern
     (This : Construction_Instance)
      return Leander.Syntax.Patterns.Reference;

   type Update_Instance (Count : Natural) is new Parent with
      record
         Value  : Reference;
         Fields : Field_Expression_Array (1 .. Count);
         Env    : Leander.Environment.Reference;
      end record;

   overriding function To_Core
     (This : Update_Instance)
      return Leander.Core.Expressions.Reference;

end Leander.Syntax.Expressions.Records;
