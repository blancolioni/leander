package Leander.Source is

   type Source_Location is private;

   function Simple_File_Name (Location : Source_Location) return String;
   function Full_File_Name (Location : Source_Location) return String;
   function Line_Number (Location : Source_Location) return Positive;
   function Column_Number (Location : Source_Location) return Positive;

   function Show (Location : Source_Location) return String;

   function Create_Location
     (File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Source_Location;

private

   type Source_File_Id is new Positive;

   type Source_Location is
      record
         File : Source_File_Id;
         Line : Positive;
         Col  : Positive;
      end record;

end Leander.Source;
