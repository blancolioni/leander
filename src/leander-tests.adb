with Ada.Command_Line;
with Ada.Text_IO;

with Leander.Core;
with Leander.Syntax;
with Leander.Tests.Evaluation;
with Leander.Tests.Expressions;
with Leander.Tests.Inference;
with Leander.Tests.Kinds;
with Leander.Tests.Prelude;
with Leander.Tests.Tycons;
with Leander.Tests.Type_Classes;
with Leander.Tests.Types;
with Leander.Tests.Tyvars;

package body Leander.Tests is

   Total_Tests : Natural := 0;
   Passed      : Natural := 0;
   Failed      : Natural := 0;
   Errors      : Natural := 0;

   procedure Report
     (Col_1, Col_2, Col_3, Col_4 : String := "");

   -----------
   -- Error --
   -----------

   procedure Error (Name, Error_Message : String) is
   begin
      Report (Name, "", "ERROR", Error_Message);
      Errors := Errors + 1;
   end Error;

   ----------
   -- Fail --
   ----------

   procedure Fail (Name, Expected, Failure_Message : String) is
   begin
      Report (Name, Expected, "FAIL", Failure_Message);
      Failed := Failed + 1;
   end Fail;

   ------------
   -- Report --
   ------------

   procedure Report
     (Col_1, Col_2, Col_3, Col_4 : String := "")
   is
      use Ada.Text_IO;
   begin
      Total_Tests := @ + 1;
      Put (Col_1);
      if Col_2 /= "" then
         Set_Col (25);
         Put (Col_2);
      end if;
      if Col_3 /= "" then
         Set_Col (50);
         Put (Col_3);
      end if;
      if Col_4 /= "" then
         Set_Col (75);
         Put (Col_4);
      end if;
      New_Line;
   end Report;

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Leander.Tests.Kinds.Run_Tests;
      Leander.Tests.Tycons.Run_Tests;
      Leander.Tests.Tyvars.Run_Tests;
      Leander.Tests.Types.Run_Tests;
      Leander.Tests.Expressions.Run_Tests;
      Leander.Tests.Inference.Run_Tests;
      Leander.Tests.Prelude.Run_Tests;
      Leander.Tests.Evaluation.Run_Tests;
      Leander.Tests.Type_Classes.Run_Tests;
      Leander.Syntax.Prune;
      Leander.Core.Prune;
      Ada.Text_IO.Put_Line
        ("Tests:" & Total_Tests'Image
         & "; passed:"
         & Passed'Image
         & "; failed"
         & Failed'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Failed));
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test (Name : String; Pass : Boolean) is
   begin
      if Pass then
         Report (Name, "", "PASS", "");
         Passed := Passed + 1;
      else
         Report (Name, "", "FAIL");
         Failed := @ + 1;
      end if;
   end Test;

   ----------
   -- Test --
   ----------

   procedure Test (Name, Expected, Found : String) is
   begin
      if Expected = Found then
         Report (Name, Expected, "PASS", "");
         Passed := Passed + 1;
      else
         Report (Name, Expected, "FAIL", Found);
         Failed := @ + 1;
      end if;
   end Test;

end Leander.Tests;
