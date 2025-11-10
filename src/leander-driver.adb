with Ada.Text_IO;

with Leander.Command_Line;
with Leander.Handles;
with Leander.Logging;
with Leander.Repl;
with Leander.Tests;
with Leander.Version;

procedure Leander.Driver is
   Core_Size : constant Natural := Command_Line.Core_Size * 1024 / 8;
begin

   if Command_Line.Version then
      Ada.Text_IO.Put_Line ("Leander " & Leander.Version.Version_String);
      return;
   end if;

   if Command_Line.Enable_Log then
      Leander.Logging.Start_Logging;
   end if;

   if Command_Line.Evaluate /= "" then
      declare
         H : Leander.Handles.Handle :=
               Leander.Handles.Create (Core_Size);
         Result : constant String :=
                    H.Evaluate (Command_Line.Evaluate);
      begin
         if Result /= "I" then
            Ada.Text_IO.Put_Line (Result);
         end if;
         H.Close;
      end;
   elsif Command_Line.Self_Test then
      Leander.Tests.Run_Tests;
   elsif Command_Line.Main /= "" then
      declare
         H      : Leander.Handles.Handle :=
                    Leander.Handles.Create (Core_Size);
      begin
         H.Load_Module (Command_Line.Main);

         declare
            Result : constant String := H.Evaluate ("runIO main");
         begin
            if Result /= "I" then
               Ada.Text_IO.Put_Line (Result);
            end if;
         end;

         if Command_Line.Report then
            H.Report;
         end if;

         H.Close;
      end;
   else
      Leander.Repl.Start (Core_Size);
   end if;

   if Command_Line.Enable_Log then
      Leander.Logging.Stop_Logging;
   end if;

exception
   when others =>
      if Command_Line.Enable_Log then
         Leander.Logging.Stop_Logging;
      end if;
      raise;
end Leander.Driver;
