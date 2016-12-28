package Leander.Source is

   type Source_Reference is private;

   function Simple_File_Name (Reference : Source_Reference) return String;
   function Full_File_Name (Reference : Source_Reference) return String;
   function Line_Number (Reference : Source_Reference) return Positive;
   function Column_Number (Reference : Source_Reference) return Positive;

   function Show (Reference : Source_Reference) return String;

   function Create_Reference
     (File_Name : String;
      Line      : Positive;
      Column    : Positive)
      return Source_Reference;

private

   type Source_File_Reference is new Positive;

   type Source_Reference is
      record
         File : Source_File_Reference;
         Line : Positive;
         Col  : Positive;
      end record;

end Leander.Source;
