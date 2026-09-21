package Leander.Errors is

   --  Whether anything has gone wrong is, oddly, not something the
   --  compiler could answer until now. Parse_Module absorbs Parse_Error,
   --  Environment.Error only writes to standard error, and Elaborate
   --  discards its inference context's failure entirely -- so a module
   --  full of mistakes loads, prints its complaints, and reports success.
   --  That is tolerable for a REPL and useless for a test that wants to
   --  assert a program is rejected.
   --
   --  GCS.Errors already tracks its own, for anything reported against a
   --  source location. This covers the rest, and Leander.Had_Errors is
   --  the union.

   procedure Note_Error;
   procedure Clear;
   function Had_Errors return Boolean;

end Leander.Errors;
