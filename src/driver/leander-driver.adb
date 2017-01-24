with GCS.Errors;

with SK.Debug;
with SK.Machine;
with SK.Machine.Assembler;

with Leander.Errors;

with Leander.Environments;

with Leander.Kinds.Trees;

with Leander.Core.Trees;
with Leander.Parser.Modules;

with Leander.Repl;

with Leander.Logging;
with Leander.Paths;

procedure Leander.Driver is
begin

   Leander.Logging.Start_Logging;

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
                  SK.Machine.Create_Machine (1024 * 1024);
   begin

      if not GCS.Errors.Has_Errors
        and then not Leander.Errors.Has_Errors
      then
         Env.Compile (Machine);
      end if;

      if not GCS.Errors.Has_Errors
        and then not Leander.Errors.Has_Errors
      then
         SK.Machine.Assembler.Push (Machine, "runIO");
         SK.Machine.Assembler.Push (Machine, SK.Initial_World);
         SK.Machine.Apply (Machine);
         SK.Machine.Assembler.Push (Machine, "selfTest");
         SK.Machine.Apply (Machine);

         if False then
            SK.Debug.Enable (SK.Debug.Eval);
            SK.Debug.Enable (SK.Debug.Compiler);
            SK.Debug.Enable (SK.Debug.Optimisation);
            SK.Debug.Enable (SK.Debug.Linker);
         end if;
         SK.Machine.Evaluate (Machine);
      end if;

      if not GCS.Errors.Has_Errors
        and then not Leander.Errors.Has_Errors
      then
         Leander.Repl.Start_Repl (Env);
      end if;
   end;

   Leander.Logging.Stop_Logging;

exception

   when others =>
      Leander.Logging.Stop_Logging;
      raise;
end Leander.Driver;
