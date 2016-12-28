package body Leander.Types.Bindings is

   ---------------------
   -- Add_Constructor --
   ---------------------

   function Add_Constructor
     (List      : in out Type_Binding_List;
      Type_Name : String;
      Con_Name  : String;
      Con_Type  : Leander.Types.Trees.Tree_Type)
      return Constructor_Binding'Class
   is
      Binding : Type_Binding renames List.Map (Type_Name);
      Con : constant Constructor_Binding := Constructor_Binding'
        (Con_Type => Con_Type,
         Index    => Binding.Constructor_Count + 1);
   begin
      Binding.Con_Map.Insert (Con_Name, Con);
      Binding.Con_Vector.Append (Con_Name);
      if not Con_Type.Is_Leaf then
         Binding.Enumeration := False;
      end if;
      return Con;
   end Add_Constructor;

   -----------------------
   -- Declare_Data_Type --
   -----------------------

   procedure Declare_Data_Type
     (List      : in out Type_Binding_List;
      Name      : String;
      Data_Type : Leander.Types.Trees.Tree_Type)
   is
   begin
      List.Map.Insert
        (Name,
         Type_Binding'
           (Algebraic   => True,
            Enumeration => True,
            Primitive   => False,
            Kind        => Data_Type.Annotation,
            Head        => Data_Type,
            others      => <>));
   end Declare_Data_Type;

   ----------------------
   -- Declare_New_Type --
   ----------------------

   procedure Declare_New_Type
     (List      : in out Type_Binding_List;
      Name      : String;
      New_Type  : Leander.Types.Trees.Tree_Type)
   is
   begin
      List.Map.Insert
        (Name,
         Type_Binding'
           (Algebraic   => True,
            Enumeration => True,
            Primitive   => False,
            Kind        => New_Type.Annotation,
            Head        => New_Type,
            others      => <>));
   end Declare_New_Type;

   ----------------------------
   -- Declare_Primitive_Type --
   ----------------------------

   procedure Declare_Primitive_Type
     (List           : in out Type_Binding_List;
      Name           : String;
      Primitive_Type : Leander.Types.Trees.Tree_Type)
   is
   begin
      List.Map.Insert
        (Name,
         Type_Binding'
           (Algebraic   => False,
            Enumeration => False,
            Primitive   => True,
            Kind        => Primitive_Type.Annotation,
            Head        => Primitive_Type,
            others      => <>));
   end Declare_Primitive_Type;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List    : in out Constructor_Binding_List;
      Name    : String;
      Binding : Constructor_Binding'Class)
   is
   begin
      List.Map.Insert (Name, Constructor_Binding (Binding));
   end Insert;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (List    : in out Type_Binding_List;
      Name    : String;
      Binding : Type_Binding'Class)
   is
   begin
      List.Map.Insert (Name, Type_Binding (Binding));
   end Insert;

end Leander.Types.Bindings;
