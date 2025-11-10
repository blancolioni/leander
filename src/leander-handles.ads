private with Leander.Environment;
private with Skit.Environment;

package Leander.Handles is

   type Handle is tagged private;

   function Create
     (Size : Natural)
      return Handle;

   function Evaluate
     (This       : in out Handle;
      Expression : String)
      return String;

   procedure Load_Module
     (This : in out Handle;
      Path : String);

   procedure Report
     (This : in out Handle);

   procedure Close (This : in out Handle);

private

   type Handle is tagged
      record
         Skit_Env : Skit.Environment.Reference;
         Env      : Leander.Environment.Reference;
      end record;

end Leander.Handles;
