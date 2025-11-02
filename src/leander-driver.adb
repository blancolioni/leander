with Leander.Logging;
with Leander.Tests;

procedure Leander.Driver is
begin
   Leander.Logging.Start_Logging;
   Leander.Tests.Run_Tests;
   Leander.Logging.Stop_Logging;
exception
   when others =>
      Leander.Logging.Stop_Logging;
      raise;
end Leander.Driver;
