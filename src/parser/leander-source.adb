with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;

with WL.String_Maps;

package body Leander.Source is

   package File_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Source_File_Reference, String);

   package File_Name_Maps is
     new WL.String_Maps (Source_File_Reference);

   File_Name_Vector : File_Name_Vectors.Vector;
   File_Name_Map    : File_Name_Maps.Map;

   -------------------
   -- Column_Number --
   -------------------

   function Column_Number (Reference : Source_Reference) return Positive is
   begin
      return Reference.Col;
   end Column_Number;

   ----------------------
   -- Create_Reference --
   ----------------------

   function Create_Reference
     (File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Source_Reference
   is
   begin
      if not File_Name_Map.Contains (File_Name) then
         File_Name_Vector.Append (File_Name);
         File_Name_Map.Insert (File_Name, File_Name_Vector.Last_Index);
      end if;
      return (File_Name_Map.Element (File_Name), Line, Column);
   end Create_Reference;

   --------------------
   -- Full_File_Name --
   --------------------

   function Full_File_Name (Reference : Source_Reference) return String is
   begin
      return File_Name_Vector.Element (Reference.File);
   end Full_File_Name;

   -----------------
   -- Line_Number --
   -----------------

   function Line_Number (Reference : Source_Reference) return Positive is
   begin
      return Reference.Line;
   end Line_Number;

   ----------
   -- Show --
   ----------

   function Show (Reference : Source_Reference) return String is
      Line_Img  : String := Positive'Image (Reference.Line);
      Col_Img   : String := Positive'Image (Reference.Col);
      File_Name : constant String := Simple_File_Name (Reference);
   begin
      Line_Img (Line_Img'First) := ':';
      Col_Img (Col_Img'First) := ':';
      return File_Name & Line_Img & Col_Img;
   end Show;

   ----------------------
   -- Simple_File_Name --
   ----------------------

   function Simple_File_Name (Reference : Source_Reference) return String is
   begin
      return Ada.Directories.Simple_Name (Full_File_Name (Reference));
   end Simple_File_Name;

end Leander.Source;
