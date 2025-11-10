with Ada.Text_IO;
with Leander.Handles;

package body Leander.Repl is

   -----------
   -- Start --
   -----------

   procedure Start (Core_Size : Natural) is
      use Ada.Text_IO;
      Handle : Leander.Handles.Handle :=
                 Leander.Handles.Create (Core_Size);
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
               else
                  Put_Line (Standard_Error, "Unimplemented");
               end if;
            else
               declare
                  T : constant String := Handle.Infer_Type (Line);
               begin
                  if T'Length > 2
                    and then T (T'First .. T'First + 1) = "IO"
                  then
                     declare
                        Result : constant String :=
                                   Handle.Evaluate ("runIO (" & Line & ")");
                     begin
                        if T /= "IO ()" then
                           Put_Line (Result);
                        end if;
                     end;
                  else
                     Put_Line (Handle.Evaluate (Line));
                  end if;
               end;
            end if;
         end;
      end loop;
   end Start;

end Leander.Repl;
