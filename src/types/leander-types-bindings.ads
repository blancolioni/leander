private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Containers.Vectors;
private with Ada.Strings.Fixed.Hash;

with Leander.Source;
with Leander.Types.Trees;

package Leander.Types.Bindings is

   type Constructor_Count_Range is new Natural;
   subtype Constructor_Index_Range is
     Constructor_Count_Range range 1 .. Constructor_Count_Range'Last;

   type Constructor_Binding is tagged private;

   function Constructor_Type
     (Binding : Constructor_Binding)
      return Leander.Types.Trees.Tree_Type;

   function Constructor_Index
     (Binding : Constructor_Binding)
      return Constructor_Index_Range;

   function Constructor_Arity
     (Binding : Constructor_Binding)
      return Natural;

   function Constructor_Field_Name
     (Binding : Constructor_Binding;
      Index   : Positive)
      return String;

   procedure Add_Component
     (Constructor    : in out Constructor_Binding'Class;
      Component_Type : Leander.Types.Trees.Tree_Type);

   procedure Add_Field
     (Constructor : in out Constructor_Binding'Class;
      Field_Name  : String;
      Field_Type  : Leander.Types.Trees.Tree_Type);

   type Constructor_Binding_List is tagged private;

   function Has_Binding
     (List : Constructor_Binding_List;
      Name : String)
      return Boolean;

   function Binding
     (List : Constructor_Binding_List;
      Name : String)
      return Constructor_Binding'Class
     with Pre => List.Has_Binding (Name);

   procedure Insert
     (List    : in out Constructor_Binding_List;
      Name    : String;
      Binding : Constructor_Binding'Class)
     with Pre => not List.Has_Binding (Name),
     Post => List.Has_Binding (Name);

   type Type_Binding is tagged private;

   function Declaration
     (Binding : Type_Binding'Class)
      return Leander.Source.Source_Reference;

   procedure Annotate_Type_Constructor
     (Binding : Type_Binding);

   function Kind (Binding : Type_Binding) return Leander.Kinds.Trees.Tree_Type;

   function Is_Algebraic (Binding : Type_Binding) return Boolean;
   function Is_Enumeration (Binding : Type_Binding) return Boolean;
   function Is_Primitive (Binding : Type_Binding) return Boolean;

   function Type_Name
     (Binding : Type_Binding)
      return String;

   function Type_Pattern
     (Binding : Type_Binding)
      return Leander.Types.Trees.Tree_Type;

   function Constructor_Count
     (Binding : Type_Binding)
      return Constructor_Count_Range
     with Pre => Binding.Is_Algebraic;

   function Constructor_Name
     (Binding : Type_Binding;
      Index   : Constructor_Index_Range)
      return String
     with Pre => Binding.Is_Algebraic
     and then Index <= Binding.Constructor_Count;

   function Constructor_Type
     (Binding : Type_Binding;
      Index   : Constructor_Index_Range)
      return Leander.Types.Trees.Tree_Type
     with Pre => Binding.Is_Algebraic
     and then Index <= Binding.Constructor_Count;

   function Constructor_Arity
     (Binding : Type_Binding;
      Index   : Constructor_Index_Range)
      return Natural
     with Pre => Binding.Is_Algebraic
     and then Index <= Binding.Constructor_Count;

   function Constructor_Field_Name
     (Binding     : Type_Binding;
      Index       : Constructor_Index_Range;
      Field_Index : Positive)
      return String
     with Pre => Binding.Is_Algebraic
     and then Index <= Binding.Constructor_Count;

   type Type_Binding_List is tagged private;

   function Has_Binding
     (List : Type_Binding_List;
      Name : String)
      return Boolean;

   function Binding
     (List : Type_Binding_List;
      Name : String)
      return Type_Binding'Class
     with Pre => List.Has_Binding (Name);

   function Variable_Binding
     (List : Type_Binding_List;
      Name : String)
      return Leander.Types.Trees.Tree_Type
     with Pre => List.Has_Binding (Name);

   procedure Insert
     (List    : in out Type_Binding_List;
      Name    : String;
      Binding : Type_Binding'Class)
     with Pre => not List.Has_Binding (Name),
     Post => List.Has_Binding (Name);

   procedure Add_Assertion
     (List      : in out Type_Binding_List;
      Name      : String;
      Assertion : Type_Assertion'Class);

   procedure Declare_Data_Type
     (List      : in out Type_Binding_List;
      Name      : String;
      Data_Type : Leander.Types.Trees.Tree_Type);

   procedure Declare_New_Type
     (List      : in out Type_Binding_List;
      Name      : String;
      New_Type  : Leander.Types.Trees.Tree_Type);

   procedure Declare_Primitive_Type
     (List           : in out Type_Binding_List;
      Name           : String;
      Primitive_Type : Leander.Types.Trees.Tree_Type);

   procedure Insert_Type_Variable
     (List  : in out Type_Binding_List;
      Name  : String;
      Value : Leander.Types.Trees.Tree_Type);

   function Add_Constructor
     (List      : in out Type_Binding_List;
      Type_Name : String;
      Con_Name  : String;
      Con_Type  : Leander.Types.Trees.Tree_Type;
      Con_Arity : Natural)
      return Constructor_Binding'Class;

   procedure Add_Constructor
     (List      : in out Type_Binding_List;
      Type_Name : String;
      Con_Name  : String;
      Con       : in out Constructor_Binding'Class);

   procedure Scan_Bindings
     (List    : Type_Binding_List;
      Process : not null access
        procedure (Name : String;
                   Binding : Type_Binding'Class));

private

   package Constructor_Field_Name_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Positive, String);

   package Constructor_Component_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Types.Trees.Tree_Type, Leander.Types.Trees."=");

   type Constructor_Binding is tagged
      record
         Con_Type   : Leander.Types.Trees.Tree_Type :=
                        Leander.Types.Trees.Empty;
         Index      : Constructor_Index_Range;
         Components : Constructor_Component_Vectors.Vector;
         Names      : Constructor_Field_Name_Vectors.Vector :=
                        Constructor_Field_Name_Vectors.Empty_Vector;
      end record;

   function Constructor_Type
     (Binding : Constructor_Binding)
      return Leander.Types.Trees.Tree_Type
   is (Binding.Con_Type);

   function Constructor_Index
     (Binding : Constructor_Binding)
      return Constructor_Index_Range
   is (Binding.Index);

   function Constructor_Arity
     (Binding : Constructor_Binding)
      return Natural
   is (Binding.Components.Last_Index);

   function Constructor_Field_Name
     (Binding : Constructor_Binding;
      Index   : Positive)
      return String
   is (Binding.Names.Element (Index));

   package Constructor_Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Constructor_Binding,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Constructor_Binding_List is tagged
      record
         Map : Constructor_Binding_Maps.Map;
      end record;

   function Has_Binding
     (List : Constructor_Binding_List;
      Name : String)
      return Boolean
   is (List.Map.Contains (Name));

   function Binding
     (List : Constructor_Binding_List;
      Name : String)
      return Constructor_Binding'Class
   is (List.Map.Element (Name));

   package Constructor_Vectors is
     new Ada.Containers.Indefinite_Vectors
       (Constructor_Index_Range, String);

   type Type_Binding is tagged
      record
         Declaration  : Leander.Source.Source_Reference;
         Algebraic    : Boolean;
         Enumeration  : Boolean;
         Primitive    : Boolean;
         Head         : Leander.Types.Trees.Tree_Type;
         Con_Map      : Constructor_Binding_Maps.Map;
         Con_Vector   : Constructor_Vectors.Vector;
      end record;

   function Kind (Binding : Type_Binding) return Leander.Kinds.Trees.Tree_Type
   is (Binding.Head.First_Leaf.Annotation);

   function Is_Algebraic (Binding : Type_Binding) return Boolean
   is (Binding.Algebraic);

   function Is_Enumeration (Binding : Type_Binding) return Boolean
   is (Binding.Enumeration);

   function Is_Primitive (Binding : Type_Binding) return Boolean
   is (Binding.Primitive);

   function Declaration
     (Binding : Type_Binding'Class)
      return Leander.Source.Source_Reference
   is (Binding.Declaration);

   function Constructor_Count
     (Binding : Type_Binding)
      return Constructor_Count_Range
   is (Binding.Con_Vector.Last_Index);

   function Constructor_Name
     (Binding : Type_Binding;
      Index   : Constructor_Index_Range)
      return String
   is (Binding.Con_Vector.Element (Index));

   function Constructor_Field_Name
     (Binding   : Type_Binding;
      Index     : Constructor_Index_Range;
      Field_Index : Positive)
      return String
   is (Binding.Con_Map.Element
       (Binding.Con_Vector (Index)).Names (Field_Index));

   function Constructor_Type
     (Binding : Type_Binding;
      Index   : Constructor_Index_Range)
      return Leander.Types.Trees.Tree_Type
   is (Binding.Con_Map.Element (Binding.Con_Vector (Index)).Constructor_Type);

   function Constructor_Arity
     (Binding : Type_Binding;
      Index   : Constructor_Index_Range)
      return Natural
   is (Binding.Con_Map.Element (Binding.Con_Vector (Index)).Constructor_Arity);

   package Type_Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Type_Binding,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Type_Binding_List is tagged
      record
         Map : Type_Binding_Maps.Map;
      end record;

   function Has_Binding
     (List : Type_Binding_List;
      Name : String)
      return Boolean
   is (List.Map.Contains (Name));

   function Binding
     (List : Type_Binding_List;
      Name : String)
      return Type_Binding'Class
   is (List.Map.Element (Name));

   function Variable_Binding
     (List : Type_Binding_List;
      Name : String)
      return Leander.Types.Trees.Tree_Type
   is (List.Map.Element (Name).Head);

   function Type_Name
     (Binding : Type_Binding)
      return String
   is (Binding.Head.First_Leaf.Constructor_Name);

   function Type_Pattern
     (Binding : Type_Binding)
      return Leander.Types.Trees.Tree_Type
   is (Binding.Head);

end Leander.Types.Bindings;
