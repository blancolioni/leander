package Leander.Names is

   type Leander_Name is private;

   function To_Leander_Name (S : String) return Leander_Name;
   function To_String (N : Leander_Name) return String;

   type Name_Array is array (Positive range <>) of Leander_Name;

   function New_Name return Leander_Name;

private

   type Leander_Name is new Positive;

end Leander.Names;
