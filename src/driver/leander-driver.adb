with SK.Debug;
with SK.Machine;
with SK.Machine.Assembler;

with Leander.Environments;

with Leander.Kinds.Trees;

with Leander.Core.Trees;
with Leander.Parser.Modules;

with Leander.Repl;

with Leander.Paths;

procedure Leander.Driver is
begin

   if False then
      SK.Debug.Enable (SK.Debug.Compiler);
      SK.Debug.Enable (SK.Debug.Optimisation);
   end if;

   declare
      Env : Leander.Environments.Environment :=
              Leander.Parser.Modules.Load_Module
                ("Prelude",
                 Leander.Paths.Config_File
                   ("libraries/Prelude.hs"));
      Machine : constant SK.Machine.SK_Machine :=
                  SK.Machine.Create_Machine (65536);
   begin
      Env.Compile (Machine);
      SK.Machine.Assembler.Push (Machine, "runIO");
      SK.Machine.Assembler.Push (Machine, SK.Initial_World);
      SK.Machine.Apply (Machine);
      SK.Machine.Assembler.Push (Machine, "testPutStrLn");
      SK.Machine.Apply (Machine);
      --        SK.Machine.Assembler.Push (Machine, "f");
--        SK.Machine.Push (Machine, SK.Initial_World);
--        SK.Machine.Apply (Machine);
--
--        SK.Machine.Assembler.Push (Machine, "w");
--        SK.Machine.Assembler.Lambda (Machine, "w");
--        SK.Machine.Assembler.Lambda (Machine, "r");
--        SK.Machine.Apply (Machine);

--        SK.Machine.Assembler.Lambda (Machine, "f");
--        SK.Machine.Apply (Machine);

      --        SK.Machine.Assembler.Lambda (Machine, "io");
--        SK.Machine.Apply (Machine);
--        SK.Machine.Assembler.Push (Machine, "x");
--        SK.Machine.Assembler.Lambda (Machine, "x");
--        SK.Machine.Apply (Machine);

      if False then
         SK.Debug.Enable (SK.Debug.Eval);
         SK.Debug.Enable (SK.Debug.Compiler);
         SK.Debug.Enable (SK.Debug.Optimisation);
         SK.Debug.Enable (SK.Debug.Linker);
      end if;
      SK.Machine.Evaluate (Machine);
      Leander.Repl.Start_Repl (Env);
   end;

end Leander.Driver;
