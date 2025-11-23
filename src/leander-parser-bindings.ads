with Leander.Syntax.Bindings;

private package Leander.Parser.Bindings is

   function At_Binding return Boolean;

   procedure Parse_Binding
     (Context : Parse_Context'Class;
      To      : Leander.Syntax.Bindings.Reference);

end Leander.Parser.Bindings;
