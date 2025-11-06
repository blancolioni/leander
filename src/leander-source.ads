package Leander.Source is

   type Source_Location is private;

   function No_Location return Source_Location;

   function Simple_File_Name (Location : Source_Location) return String;
   function Full_File_Name (Location : Source_Location) return String;
   function Line_Number (Location : Source_Location) return Natural;
   function Column_Number (Location : Source_Location) return Positive;

   function Show (Location : Source_Location) return String;

   function Create_Location
     (File_Name : String;
      Line      : Natural;
      Column    : Positive)
      return Source_Location;

   type Has_Source_Location is interface;

   function Location
     (This : Has_Source_Location)
      return Source_Location
      is abstract;

   function Show_Location
     (This : Has_Source_Location'Class)
      return String;

   procedure Error
     (This    : Has_Source_Location'Class;
      Message : String);

private

   type Source_File_Id is new Natural;

   type Source_Location is
      record
         File : Source_File_Id;
         Line : Natural;
         Col  : Positive;
      end record;

   function Show_Location
     (This : Has_Source_Location'Class)
      return String
   is (Show (Location (This)));

   function No_Location return Source_Location
   is (0, 0, 1);

end Leander.Source;
