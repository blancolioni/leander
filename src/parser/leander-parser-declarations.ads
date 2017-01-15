with Leander.Environments;

private package Leander.Parser.Declarations is

   procedure Parse_Declaration
     (Env : in out Leander.Environments.Environment);

   procedure Parse_Value_Bindings
     (Env : in out Leander.Environments.Environment);

end Leander.Parser.Declarations;
