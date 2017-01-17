with SK.Machine;
with SK.Machine.Assembler;

with Leander.Environments;

with Leander.Kinds.Trees;

with Leander.Core.Trees;
with Leander.Parser.Modules;

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
   SK.Machine.Assembler.Push (Machine, "testPutChar");
   SK.Machine.Push (Machine, SK.Initial_World);
   SK.Machine.Apply (Machine);
   SK.Machine.Evaluate (Machine);
   Leander.Repl.Start_Repl (Env);
end Leander.Driver;
