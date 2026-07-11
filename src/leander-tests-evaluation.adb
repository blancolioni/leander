with Ada.Exceptions;
with Ada.Text_IO;

package body Leander.Tests.Evaluation is

   procedure Test
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Handle         : Leander.Handle);

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Handle : Leander.Handle :=
                 Leander.Create (256 * 1024);
   begin
      Test ("1", "Int", "1", Handle);
      Test ("null []", "Bool", "K", Handle);
      Test ("null [1]", "Bool", "K I", Handle);
      Test ("length []", "Int", "0", Handle);
      Test ("length [1]", "Int", "1", Handle);
      Test ("length [1,2,3,4]", "Int", "4", Handle);
      Test ("zero 0", "Bool", "K", Handle);
      Test ("zero 1", "Bool", "K I", Handle);
      Test ("zero 100", "Bool", "K I", Handle);
      Test ("small 3", "Bool", "K", Handle);
      Test ("small 5", "Bool", "K I", Handle);
      Test ("id 123", "Int", "123", Handle);
      Test ("const 3 []", "Int", "3", Handle);
      Test ("2 * 3 + 4", "Int", "10", Handle);
      Test ("2 + 3 * 4", "Int", "14", Handle);
      Test ("2 - 3 * 4", "Int", "-10", Handle);
      Test ("seq (#trace 42) 4", "Int", "4", Handle);
      Test ("sum [1,2,3,4]", "Int", "10", Handle);
      Test ("sum (map (+1) [1,2,3])", "Int", "9", Handle);
      Test ("length (take 10 [1,2,3])", "Int", "3", Handle);
      Test ("length (take 2 [1,2,3])", "Int", "2", Handle);
      --  Test ("sum (do { x <- [42]; return x })", "Int", "42", Env, Prelude);
      --  Test ("sum (do { let x = 42; return x })", "Int", "42", Env, Prelude);
      Test ("maybe 42 id Nothing", "Int", "42", Handle);
      Test ("maybe 0 sum (Just [1,2,3])", "Int", "6", Handle);
      Test ("fst (123,456)", "Int", "123", Handle);
      Test ("snd (123,456)", "Int", "456", Handle);
      Test ("(*2) 3", "Int", "6", Handle);
      Test ("length ""Hello""", "Int", "5", Handle);
      Test ("runIO (putChar 'A')", "()", "I", Handle);
      Test ("runIO (putStr ""Hello, world!\n"")", "()", "I", Handle);

      Handle.Report;
      Handle.Close;
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test
     (Expression     : String;
      Expected_Type  : String;
      Expected_Value : String;
      Handle         : Leander.Handle)
   is
   begin
      declare
         Inferred_Type : constant String := Handle.Infer_Type (Expression);
      begin
         if Inferred_Type /= Expected_Type then
            Fail (Expression, Expected_Type, Inferred_Type);
         else
            declare
               Value : constant String := Handle.Evaluate (Expression);
            begin
               Test (Expression, Expected_Value, Value);
            end;
         end if;
      end;
   exception
      when E : others =>
         Error (Expression, Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Ada.Exceptions.Exception_Information (E));
   end Test;

end Leander.Tests.Evaluation;
