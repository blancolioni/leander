package body Leander.Errors is

   Seen : Boolean := False;

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Seen := False;
   end Clear;

   ----------------
   -- Had_Errors --
   ----------------

   function Had_Errors return Boolean is (Seen);

   ----------------
   -- Note_Error --
   ----------------

   procedure Note_Error is
   begin
      Seen := True;
   end Note_Error;

end Leander.Errors;
