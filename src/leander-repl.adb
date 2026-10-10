with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Leander.Repl is

   -----------
   -- Start --
   -----------

   procedure Start (Core_Size : Natural) is
      use Ada.Text_IO;
      Handle : Leander.Handle :=
                 Leander.Create (Core_Size);
   begin
      while True loop
         Put (Handle.Current_Environment & "> ");
         Flush;
         declare
            Line : constant String := Get_Line;
         begin
            exit when Line = ":quit";
            if Line (Line'First) = ':' then
               if Line = ":report" then
                  Handle.Report;
               elsif Line'Length > 6
                 and then Line (Line'First .. Line'First + 5) = ":type "
               then
                  Put_Line
                    (Handle.Infer_Type (Line (Line'First + 6 .. Line'Last)));
               elsif Line'Length > 9
                 and then Line (Line'First .. Line'First + 8) = ":compile "
               then
                  Put_Line
                    (Handle.Compile (Line (Line'First + 9 .. Line'Last)));
               --  elsif Line = ":trace" then
               --     Handle.Trace (True);
               else
                  Put_Line (Standard_Error, "Unimplemented");
               end if;
            else
               declare
                  T : constant String := Handle.Infer_Type (Line);
               begin
                  if Ada.Strings.Fixed.Index (T, "IO") > 0 then
                     declare
                        Result : constant String :=
                                   Handle.Evaluate ("runIO (" & Line & ")");
                     begin
                        if T /= "IO ()" then
                           Put_Line (Result);
                        end if;
                     end;
                  else
                     declare
                        Result : constant String :=
                                   Handle.Evaluate
                                     ("runIO (print (" & Line & "))");
                     begin
                        pragma Unreferenced (Result);
                     end;
                  end if;
               end;
            end if;
         exception
            when Leander.Compile_Error =>
               --  Already reported; carry on with the next line.
               null;
            when E : Leander.Runtime_Error =>
               Put_Line
                 (Standard_Error, Ada.Exceptions.Exception_Message (E));

               --  An error raised inside a primitive leaves the machine
               --  mid-evaluation, and Skit cannot yet recover from that
               --  (blancolioni/skit#36).
               --  The REPL holds nothing but the Prelude, so start again.
               Handle.Close;
               Handle := Leander.Create (Core_Size);
         end;
      end loop;
      Handle.Close;

   end Start;

end Leander.Repl;
