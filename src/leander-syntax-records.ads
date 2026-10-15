with Leander.Data_Types;
with Leander.Environment;
with Leander.Source;

package Leander.Syntax.Records is

   --  What the record forms of expressions and patterns need to find out
   --  about a constructor's fields (Haskell 2010, sections 3.15 and 3.17;
   --  #33).  They ask only once the module's declarations are all in, so
   --  that a type may be declared after the code that uses its labels.

   function Constructor_Type
     (Env  : Leander.Environment.Reference;
      Name : String)
      return Leander.Data_Types.Reference;
   --  The data type the constructor Name belongs to, or null if Name is
   --  not a constructor in scope.  Name may be qualified.

   function Constructor_Index
     (DT   : Leander.Data_Types.Reference;
      Name : String)
      return Natural;
   --  Which of DT's constructors Name is, qualified or not; 0 if none.

   function Bare_Name (Name : String) return String;
   --  Name without its module qualifier.

   function Qualifier (Name : String) return String;
   --  Name's module qualifier, or "" if it has none.

   procedure Report
     (Location : Leander.Source.Source_Location;
      Message  : String);
   --  Report an error at Location, which counts against the module.

end Leander.Syntax.Records;
