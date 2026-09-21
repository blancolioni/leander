package body Leander.Syntax.Modules is

   ---------------------------
   -- Add_Constructor_Export --
   ---------------------------

   procedure Add_Constructor_Export
     (This        : in out Export_List'Class;
      Type_Name   : String;
      Constructor : String)
   is
   begin
      if This.Entries.Is_Empty
        or else This.Entries.Last_Element.Name /= Type_Name
      then
         This.Add_Export (Type_Name);
      end if;

      declare
         Last : Export_Entry := This.Entries.Last_Element;
      begin
         Last.Constructors.Append (Constructor);
         This.Entries.Replace_Element (This.Entries.Last_Index, Last);
      end;
   end Add_Constructor_Export;

   ----------------
   -- Add_Export --
   ----------------

   procedure Add_Export
     (This : in out Export_List'Class;
      Name : String)
   is
   begin
      This.Entries.Append
        (Export_Entry'(Name              => To_Unbounded_String (Name),
                       With_Constructors => False,
                       Constructors      => <>));
   end Add_Export;

   --------------
   -- Add_Name --
   --------------

   procedure Add_Name
     (This : in out Import_Declaration'Class;
      Name : String)
   is
   begin
      This.Names.Append (Name);
   end Add_Name;

   ---------------------
   -- Add_Type_Export --
   ---------------------

   procedure Add_Type_Export
     (This              : in out Export_List'Class;
      Name              : String;
      With_Constructors : Boolean)
   is
   begin
      This.Entries.Append
        (Export_Entry'(Name              => To_Unbounded_String (Name),
                       With_Constructors => With_Constructors,
                       Constructors      => <>));
   end Add_Type_Export;

   -----------
   -- Alias --
   -----------

   function Alias (This : Import_Declaration'Class) return String is
   begin
      return (if This.Alias_Name = Null_Unbounded_String
              then To_String (This.Module_Name)
              else To_String (This.Alias_Name));
   end Alias;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (This        : Export_List'Class;
      Index       : Positive;
      Constructor : Positive)
      return String
   is (This.Entries.Element (Index).Constructors.Element (Constructor));

   -----------------------
   -- Constructor_Count --
   -----------------------

   function Constructor_Count
     (This  : Export_List'Class;
      Index : Positive)
      return Natural
   is (Natural (This.Entries.Element (Index).Constructors.Length));

   -----------
   -- Count --
   -----------

   function Count (This : Export_List'Class) return Natural
   is (Natural (This.Entries.Length));

   --------------------------
   -- Exports_Constructors --
   --------------------------

   function Exports_Constructors
     (This  : Export_List'Class;
      Index : Positive)
      return Boolean
   is (This.Entries.Element (Index).With_Constructors);

   --------------------
   -- Has_Name_List --
   --------------------

   function Has_Name_List (This : Import_Declaration'Class) return Boolean
   is (This.Name_List);

   ---------------
   -- Is_Hiding --
   ---------------

   function Is_Hiding (This : Import_Declaration'Class) return Boolean
   is (This.Hiding);

   ----------------
   -- Is_Present --
   ----------------

   function Is_Present (This : Export_List'Class) return Boolean
   is (This.Present);

   ------------------
   -- Is_Qualified --
   ------------------

   function Is_Qualified (This : Import_Declaration'Class) return Boolean
   is (This.Qualified);

   ------------------
   -- Is_Selective --
   ------------------

   function Is_Selective (This : Import_Declaration'Class) return Boolean
   is (This.Qualified
       or else This.Name_List
       or else This.Alias_Name /= Null_Unbounded_String);

   -----------------
   -- Module_Name --
   -----------------

   function Module_Name (This : Import_Declaration'Class) return String
   is (To_String (This.Module_Name));

   ----------
   -- Name --
   ----------

   function Name
     (This  : Export_List'Class;
      Index : Positive)
      return String
   is (To_String (This.Entries.Element (Index).Name));

   ----------
   -- Name --
   ----------

   function Name
     (This  : Import_Declaration'Class;
      Index : Positive)
      return String
   is (This.Names.Element (Index));

   ----------------
   -- Name_Count --
   ----------------

   function Name_Count (This : Import_Declaration'Class) return Natural
   is (Natural (This.Names.Length));

   ---------------
   -- Set_Alias --
   ---------------

   procedure Set_Alias
     (This  : in out Import_Declaration'Class;
      Alias : String)
   is
   begin
      This.Alias_Name := To_Unbounded_String (Alias);
   end Set_Alias;

   -----------------------
   -- Start_Export_List --
   -----------------------

   procedure Start_Export_List (This : in out Export_List'Class) is
   begin
      This.Present := True;
   end Start_Export_List;

   ------------------
   -- Start_Import --
   ------------------

   procedure Start_Import
     (This        : in out Import_Declaration'Class;
      Module_Name : String;
      Qualified   : Boolean)
   is
   begin
      This.Module_Name := To_Unbounded_String (Module_Name);
      This.Qualified := Qualified;
   end Start_Import;

   ---------------------
   -- Start_Name_List --
   ---------------------

   procedure Start_Name_List
     (This   : in out Import_Declaration'Class;
      Hiding : Boolean)
   is
   begin
      This.Name_List := True;
      This.Hiding := Hiding;
   end Start_Name_List;

end Leander.Syntax.Modules;
