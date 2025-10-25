with Ada.Containers.Indefinite_Vectors;

package body Leander.Names is

   Next_Name : Natural := 0;

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Leander_Name, String);

   Name_Vector : Name_Vectors.Vector;

   --------------
   -- New_Name --
   --------------

   function New_Name return Leander_Name is
      Img : String := Next_Name'Image;
   begin
      Img (Img'First) := '_';
      Next_Name := Next_Name + 1;
      return To_Leander_Name (Img);
   end New_Name;

   ---------------------
   -- To_Leander_Name --
   ---------------------

   function To_Leander_Name (S : String) return Leander_Name is
   begin
      for N in 1 .. Name_Vector.Last_Index loop
         if Name_Vector (N) = S then
            return N;
         end if;
      end loop;
      Name_Vector.Append (S);
      return Name_Vector.Last_Index;
   end To_Leander_Name;

   ---------------
   -- To_String --
   ---------------

   function To_String (N : Leander_Name) return String is
   begin
      return Name_Vector (N);
   end To_String;

end Leander.Names;
