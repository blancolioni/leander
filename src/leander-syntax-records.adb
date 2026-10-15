with Ada.Strings.Fixed;

with Leander.Core;
with Leander.Errors;
with Leander.Names;

package body Leander.Syntax.Records is

   use type Leander.Environment.Reference;

   ---------------
   -- Bare_Name --
   ---------------

   function Bare_Name (Name : String) return String is
      Dot : constant Natural :=
              Ada.Strings.Fixed.Index (Name, ".", Ada.Strings.Backward);
   begin
      if Dot > Name'First
        and then Dot < Name'Last
        and then Name (Name'First) in 'A' .. 'Z'
      then
         return Name (Dot + 1 .. Name'Last);
      else
         return Name;
      end if;
   end Bare_Name;

   ---------------
   -- Qualifier --
   ---------------

   function Qualifier (Name : String) return String is
      Bare : constant String := Bare_Name (Name);
   begin
      if Bare'Length = Name'Length then
         return "";
      else
         return Name (Name'First .. Name'Last - Bare'Length - 1);
      end if;
   end Qualifier;

   -----------------------
   -- Constructor_Index --
   -----------------------

   function Constructor_Index
     (DT   : Leander.Data_Types.Reference;
      Name : String)
      return Natural
   is
   begin
      return DT.Constructor_Index (Leander.Core.To_Conid (Bare_Name (Name)));
   end Constructor_Index;

   ----------------------
   -- Constructor_Type --
   ----------------------

   function Constructor_Type
     (Env  : Leander.Environment.Reference;
      Name : String)
      return Leander.Data_Types.Reference
   is
   begin
      if Env /= null
        and then Env.Exists
          (Leander.Names.To_Leander_Name (Name),
           Leander.Environment.Constructor)
      then
         return Env.Con_Data_Type (Leander.Core.To_Conid (Name));
      else
         return null;
      end if;
   end Constructor_Type;

   ------------
   -- Report --
   ------------

   procedure Report
     (Location : Leander.Source.Source_Location;
      Message  : String)
   is
   begin
      Leander.Errors.Report
        (Leander.Source.Simple_File_Name (Location),
         Leander.Source.Line_Number (Location),
         Leander.Source.Column_Number (Location),
         Message);
   end Report;

end Leander.Syntax.Records;
