package body Leander.Types.Bindings is

   -------------------
   -- Add_Assertion --
   -------------------

   procedure Add_Assertion
     (List      : in out Type_Binding_List;
      Name      : String;
      Assertion : Type_Assertion'Class)
   is
   begin
      List.Map.Element (Name).Head.First_Leaf.Update_Node.Add_Assertion
        (Assertion);
   end Add_Assertion;

   -------------------
   -- Add_Component --
   -------------------

   procedure Add_Component
     (Constructor    : in out Constructor_Binding'Class;
      Component_Type : Leander.Types.Trees.Tree_Type)
   is
   begin
      Constructor.Components.Append (Component_Type);
      Constructor.Names.Append ("");
   end Add_Component;

   ---------------------
   -- Add_Constructor --
   ---------------------

   function Add_Constructor
     (List      : in out Type_Binding_List;
      Type_Name : String;
      Con_Name  : String;
      Con_Type  : Leander.Types.Trees.Tree_Type;
      Con_Arity : Natural)
      return Constructor_Binding'Class
   is
      Binding : Type_Binding renames List.Map (Type_Name);
      Con : Constructor_Binding := Constructor_Binding'
        (Con_Type => Con_Type,
         Index    => Binding.Constructor_Count + 1,
         others   => <>);
      It      : Leander.Types.Trees.Tree_Type := Con_Type;
   begin
      for I in 1 .. Con_Arity loop
         Con.Names.Append ("");
         Con.Components.Append (It.Left.Right);
         It := It.Right;
      end loop;

      Binding.Con_Map.Insert (Con_Name, Con);
      Binding.Con_Vector.Append (Con_Name);
      if not Con_Type.Is_Leaf then
         Binding.Enumeration := False;
      end if;
      return Con;
   end Add_Constructor;

   ---------------------
   -- Add_Constructor --
   ---------------------

   procedure Add_Constructor
     (List      : in out Type_Binding_List;
      Type_Name : String;
      Con_Name  : String;
      Con       : in out Constructor_Binding'Class)
   is
      Binding : Type_Binding renames List.Map (Type_Name);
   begin
      Con.Con_Type := Binding.Type_Pattern;
      for Component of reverse Con.Components loop
         Con.Con_Type :=
           Leander.Types.Trees.Leaf
             (Leander.Types.Constructor ("->"))
               .Apply (Component).Apply (Con.Con_Type);
      end loop;
      Binding.Con_Map.Insert (Con_Name, Constructor_Binding (Con));
      Binding.Con_Vector.Append (Con_Name);
      Con.Index := Binding.Con_Vector.Last_Index;
      if not Con.Con_Type.Is_Leaf then
         Binding.Enumeration := False;
      end if;

   end Add_Constructor;

   ---------------
   -- Add_Field --
   ---------------

   procedure Add_Field
     (Constructor : in out Constructor_Binding'Class;
      Field_Name  : String;
      Field_Type  : Leander.Types.Trees.Tree_Type)
   is
   begin
      Constructor.Components.Append (Field_Type);
      Constructor.Names.Append (Field_Name);
   end Add_Field;

   -------------------------------
   -- Annotate_Type_Constructor --
   -------------------------------

   procedure Annotate_Type_Constructor
     (Binding : Type_Binding)
   is
      Kind : Leander.Kinds.Trees.Tree_Type :=
               Leander.Kinds.Trees.Leaf
                 (Leander.Kinds.Primitive);
      It   : Leander.Types.Trees.Tree_Type :=
               Binding.Head;
   begin
      if not Binding.Head.Has_Annotation then
         while It.Is_Application loop
            It.Right.Set_Annotation
              (Leander.Kinds.Trees.Leaf
                 (Leander.Kinds.Primitive));
            Kind :=
              Leander.Kinds.Trees.Apply
                (Leander.Kinds.Trees.Apply
                   (Leander.Kinds.Map,
                    Leander.Kinds.Primitive),
                 Kind);
            It.Set_Annotation (Kind);
            It := It.Left;
         end loop;

         It.Set_Annotation (Kind);
      end if;
   end Annotate_Type_Constructor;

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

   --------------------------
   -- Insert_Type_Variable --
   --------------------------

   procedure Insert_Type_Variable
     (List  : in out Type_Binding_List;
      Name  : String;
      Value : Leander.Types.Trees.Tree_Type)
   is
   begin
      List.Map.Insert
        (Name,
         Type_Binding'
           (Algebraic   => False,
            Enumeration => False,
            Primitive   => False,
            Head        => Value,
            others      => <>));
   end Insert_Type_Variable;

   -------------------
   -- Scan_Bindings --
   -------------------

   procedure Scan_Bindings
     (List    : Type_Binding_List;
      Process : not null access
        procedure (Name : String;
                   Binding : Type_Binding'Class))
   is
   begin
      for Position in List.Map.Iterate loop
         Process
           (Type_Binding_Maps.Key (Position),
            Type_Binding_Maps.Element (Position));
      end loop;
   end Scan_Bindings;

end Leander.Types.Bindings;
