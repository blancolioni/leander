private package Leander.Parser.Modules is

   function Scan_Module_Header return String;
   --  Consume the "module <name>" that opens a source file and return the
   --  dotted module name. Must be called on a freshly opened file, before
   --  Parse_Module, which continues from the export list or the "where".
   --  Returns "" after reporting the error if the file does not begin with
   --  a module header.

   function Parse_Module
     (Context  : in out Parse_Context'Class;
      Name     : String;
      From_Dir : String)
     return Leander.Environment.Reference;
   --  Parse the rest of the module whose header Scan_Module_Header has
   --  already consumed. Name is that header's name, which is what the
   --  module's environment is called. From_Dir is the directory holding
   --  this module's own source, which is the first place an import
   --  declaration looks for the module it names.

end Leander.Parser.Modules;
