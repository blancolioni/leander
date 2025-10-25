--  with Leander.Tests.Inference;
with Ada.Text_IO;
with Leander.Calculus;
with Leander.Logging;
with Leander.Tests;
with Skit;
with Skit.Compiler;
with Skit.Debug;
with Skit.Impl;
with Skit.Machine;

procedure Leander.Driver is
   use Leander.Calculus;
   T : constant Tree := Apply (Lambda ("x", Symbol ("x")), Number (1));
   M : constant Skit.Machine.Reference := Skit.Impl.Machine (4096);
begin
   Leander.Logging.Start_Logging;
   Compile (T, M);
   Skit.Compiler.Compile (M);
   M.Evaluate;
   Ada.Text_IO.Put_Line (Skit.Debug.Image (M.Top));
   Leander.Tests.Run_Tests;
   Leander.Logging.Stop_Logging;
exception
   when others =>
      Leander.Logging.Stop_Logging;
      raise;
end Leander.Driver;
