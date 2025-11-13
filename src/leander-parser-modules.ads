private package Leander.Parser.Modules is

   function Parse_Module
     (Context : in out Parse_Context'Class;
      Name    : String)
     return Leander.Environment.Reference;

end Leander.Parser.Modules;
