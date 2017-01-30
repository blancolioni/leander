with GCS.Errors;

with SK.Debug;
with SK.Machine;
with SK.Objects;

with Leander.Errors;

with Leander.Environments;
with Leander.Primitives;

with Leander.Kinds.Trees;

with Leander.Core.Trees;
with Leander.Parser.Modules;

with Leander.Repl;

with Leander.Logging;
with Leander.Paths;

procedure Leander.Driver is
begin

   Leander.Logging.Start_Logging;

   declare
      Env : Leander.Environments.Environment :=
              Leander.Parser.Modules.Load_Module
                ("Prelude",
                 Leander.Paths.Config_File
                   ("libraries/Prelude.hs"));
      Machine : constant SK.Machine.SK_Machine :=
                  SK.Machine.Create (1024 * 1024);
   begin

      Leander.Primitives.Load_SK_Primitives (Machine.all);

      if not GCS.Errors.Has_Errors
        and then not Leander.Errors.Has_Errors
      then
         Env.Compile (Machine);
      end if;

      if not GCS.Errors.Has_Errors
        and then not Leander.Errors.Has_Errors
      then
         Machine.Push ("runIO");
         Machine.Push (SK.Objects.Initial_World);
         Machine.Apply;
         Machine.Push ("selfTest");
         Machine.Apply;

         if False then
            SK.Debug.Enable (SK.Debug.Eval);
         end if;

         Machine.Compile;
         Machine.Link;
         Machine.Evaluate;
      end if;

      Machine.Report_State;

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
