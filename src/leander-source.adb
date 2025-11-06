with Ada.Containers.Indefinite_Vectors;
with Ada.Directories;

with Ada.Text_IO;
with WL.String_Maps;

package body Leander.Source is

   package File_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Source_File_Id, String);

   package File_Name_Maps is
     new WL.String_Maps (Source_File_Id);

   File_Name_Vector : File_Name_Vectors.Vector;
   File_Name_Map    : File_Name_Maps.Map;

   -------------------
   -- Column_Number --
   -------------------

   function Column_Number (Location : Source_Location) return Positive is
   begin
      return Location.Col;
   end Column_Number;

   ----------------------
   -- Create_Location --
   ----------------------

   function Create_Location
     (File_Name : String;
      Line      : Natural;
      Column    : Positive)
      return Source_Location
   is
   begin
      if not File_Name_Map.Contains (File_Name) then
         File_Name_Vector.Append (File_Name);
         File_Name_Map.Insert (File_Name, File_Name_Vector.Last_Index);
      end if;
      return (File_Name_Map.Element (File_Name), Line, Column);
   end Create_Location;

   -----------
   -- Error --
   -----------

   procedure Error
     (This    : Has_Source_Location'Class;
      Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         This.Show_Location & ": " & Message);
   end Error;

   --------------------
   -- Full_File_Name --
   --------------------

   function Full_File_Name (Location : Source_Location) return String is
   begin
      return File_Name_Vector.Element (Location.File);
   end Full_File_Name;

   -----------------
   -- Line_Number --
   -----------------

   function Line_Number (Location : Source_Location) return Natural is
   begin
      return Location.Line;
   end Line_Number;

   ----------
   -- Show --
   ----------

   function Show (Location : Source_Location) return String is
   begin
      if Location.File = 0 then
         return "internal";
      end if;
      declare
         Line_Img  : String := Positive'Image (Location.Line);
         Col_Img   : String := Positive'Image (Location.Col);
         File_Name : constant String := Simple_File_Name (Location);
      begin
         Line_Img (Line_Img'First) := ':';
         Col_Img (Col_Img'First) := ':';
         return File_Name & Line_Img & Col_Img;
      end;
   end Show;

   ----------------------
   -- Simple_File_Name --
   ----------------------

   function Simple_File_Name (Location : Source_Location) return String is
   begin
      return Ada.Directories.Simple_Name (Full_File_Name (Location));
   end Simple_File_Name;

begin
   File_Name_Vector.Append ("internal");
   File_Name_Map.Insert ("internal", 0);
end Leander.Source;
