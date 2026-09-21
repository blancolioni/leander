private package Leander.Parser.Modules is

   function Scan_Module_Header return String;
   --  Consume the "module <name>" that opens a source file and return the
   --  dotted module name. Must be called on a freshly opened file, before
   --  Parse_Module, which continues from the "where". Returns "" after
   --  reporting the error if the file does not begin with a module header.

   function Parse_Module
     (Context     : in out Parse_Context'Class;
      Name        : String;
      Prelude_Env : Leander.Environment.Reference := null)
     return Leander.Environment.Reference;
   --  Parse the rest of the module whose header Scan_Module_Header has
   --  already consumed. Name is that header's name, which is what the
   --  module's environment is called.

end Leander.Parser.Modules;
