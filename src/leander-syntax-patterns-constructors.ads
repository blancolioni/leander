with Leander.Names;

private package Leander.Syntax.Patterns.Constructors is

   subtype Parent is Leander.Syntax.Patterns.Instance;
   type Instance (<>) is new Parent with private;

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String;
      Args     : Reference_Array)
      return Reference;

private

   type Instance (Arg_Count : Natural) is new Parent with
      record
         Name  : Leander.Names.Leander_Name;
         Args  : Reference_Array (1 .. Arg_Count);
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
      return Reference_Array
   is (This.Args);

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Patterns.Reference;

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String;
      Args     : Reference_Array)
      return Reference
   is (new Instance'(Args'Length, Location,
       Leander.Names.To_Leander_Name (Name), Args));

end Leander.Syntax.Patterns.Constructors;
