with Ada.Directories;
with Ada.Text_IO;

with Leander.Command_Line;
with Leander.Repl;
with Leander.Tests;
with Leander.Version;

procedure Leander.Driver is
   Core_Size : constant Natural := Command_Line.Core_Size * 1024;
begin

   if Command_Line.Version then
      Ada.Text_IO.Put_Line ("Leander " & Leander.Version.Version_String);
      return;
   end if;

   if Command_Line.Evaluate /= "" then
      declare
         H : Leander.Handle := Leander.Create (Core_Size);
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
   elsif Command_Line.Precompile /= "" then
      declare
         Source_Path : constant String := Command_Line.Precompile;
         Module_Name : constant String :=
                         Ada.Directories.Base_Name (Source_Path);
         Image_Path  : constant String :=
                         Ada.Directories.Compose
                           (Ada.Directories.Containing_Directory
                              (Ada.Directories.Full_Name (Source_Path)),
                            Module_Name, "skix");
         H : Leander.Handle := Leander.Create (Core_Size);
      begin
         H.Load_Module (Source_Path);
         H.Dump_Module (Image_Path, Module_Name);
         Ada.Text_IO.Put_Line ("Wrote " & Image_Path);
         H.Close;
      end;
   elsif Command_Line.Main /= "" then
      declare
         H : Leander.Handle := Leander.Create (Core_Size);
      begin
         H.Load_Module (Command_Line.Main);
         H.Execute ("main");
         if Command_Line.Report then
            H.Report;
         end if;

         H.Close;
      end;
   else
      Leander.Repl.Start (Core_Size);
   end if;
end Leander.Driver;
