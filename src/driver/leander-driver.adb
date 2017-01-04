with SK.Machine;

with Leander.Environments;

with Leander.Kinds.Trees;

with Leander.Core.Trees;
with Leander.Parser.Modules;
with Leander.Syntax;

with Leander.Repl;

with Leander.Paths;

procedure Leander.Driver is
   Env : Leander.Environments.Environment :=
           Leander.Parser.Modules.Load_Module
             ("Prelude",
              Leander.Paths.Config_File
                ("libraries/Prelude.hs"));
   Machine : constant SK.Machine.SK_Machine :=
               SK.Machine.Create_Machine (65536);
begin
   Env.Compile (Machine);
   Leander.Repl.Start_Repl (Env);
end Leander.Driver;
