private package Leander.Syntax.Patterns.Records is

   --  C { f1 = p1, ... } (Haskell 2010, section 3.17.1; #33).

   subtype Parent is Leander.Syntax.Patterns.Instance;
   type Instance (<>) is new Parent with private;

   function Record_Pattern
     (Location    : Leander.Source.Source_Location;
      Constructor : String;
      Fields      : Field_Pattern_Array;
      Env         : Leander.Environment.Reference)
      return Reference;

private

   type Flag_Access is access Boolean;

   type Instance (Count : Natural) is new Parent with
      record
         Name     : Leander.Names.Leander_Name;
         Fields   : Field_Pattern_Array (1 .. Count);
         Env      : Leander.Environment.Reference;
         Reported : Flag_Access;
      end record;

   overriding function Is_Constructor
     (This : Instance)
      return Boolean
   is (True);

   overriding function Constructor_Name
     (This : Instance)
      return String
   is (Leander.Names.To_String (This.Name));

   overriding function Constructor_Args
     (This : Instance)
      return Reference_Array;
   --  The field patterns in their constructor's positions, with a wildcard
   --  for each field not named.  Before the constructor's type is known
   --  (while the module is still being parsed) they are the field patterns
   --  in the order written, which binds the same variables.

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Patterns.Reference;

end Leander.Syntax.Patterns.Records;
