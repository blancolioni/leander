package Leander.Tests is

   procedure Run_Tests;

private

   procedure Test (Name, Expected, Found : String);
   procedure Test (Name : String; Pass : Boolean);
   procedure Fail (Name, Expected, Failure_Message : String);
   procedure Error (Name, Error_Message : String);

end Leander.Tests;
