package Leander.Errors is

   --  Whether anything has gone wrong is, oddly, not something the
   --  compiler could answer until now. Parse_Module absorbs Parse_Error,
   --  Environment.Error only writes to standard error, and Elaborate
   --  discards its inference context's failure entirely -- so a module
   --  full of mistakes loads, prints its complaints, and reports success.
   --  That is tolerable for a REPL and useless for a test that wants to
   --  assert a program is rejected.
   --
   --  Report is for anything with a source position; Note_Error is for
   --  failures that have already been described some other way. Either
   --  one makes Had_Errors true.

   procedure Report
     (File_Name  : String;
      Line       : Natural;
      Column     : Natural;
      Message    : String;
      Is_Warning : Boolean := False);
   --  Write "file:line:column: message" to standard error. A zero Line or
   --  Column is left out, and so is an empty File_Name. A warning is
   --  written with "warning: " in front and is not counted as an error.

   procedure Note_Error;
   procedure Clear;
   function Had_Errors return Boolean;

end Leander.Errors;
