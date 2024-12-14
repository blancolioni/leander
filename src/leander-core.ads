package Leander.Core is

   type Name_Id is private;

   function Id (S : String) return Name_Id;
   function Show (Id : Name_Id) return String;

private

   type Name_Id is new Positive;

end Leander.Core;
