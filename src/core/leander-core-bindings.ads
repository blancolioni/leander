private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash;

with Leander.Core.Trees;

package Leander.Core.Bindings is

   type Binding_List is tagged private;

   function Has_Binding
     (List : Binding_List;
      Name : String)
      return Boolean;

   function Has_Value
     (List : Binding_List;
      Name : String)
      return Boolean;

   function Has_Signature
     (List : Binding_List;
      Name : String)
      return Boolean;

   function Bound_Name
     (List : Binding_List;
      Name : String)
      return String
     with Pre => List.Has_Binding (Name);

   function Binding
     (List : Binding_List;
      Name : String)
      return Leander.Core.Trees.Tree_Type
     with Pre => List.Has_Binding (Name);

   function Signature
     (List : Binding_List;
      Name : String)
      return Leander.Types.Trees.Tree_Type
     with Pre => List.Has_Signature (Name);

   procedure Insert
     (List       : in out Binding_List;
      Name       : String;
      Binding    : Leander.Core.Trees.Tree_Type;
      Bound_Name : String := "")
     with Pre => not List.Has_Value (Name),
     Post => List.Has_Value (Name);

   procedure Insert
     (List          : in out Binding_List;
      Name          : String;
      Signature     : Leander.Types.Trees.Tree_Type)
     with Pre => not List.Has_Signature (Name),
     Post => List.Has_Signature (Name);

   procedure Scan
     (List    : Binding_List;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type));

   procedure Scan
     (List    : Binding_List;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type;
                   Signature : Leander.Types.Trees.Tree_Type));

private

   type Value_Binding_Record is
      record
         Has_Value     : Boolean := False;
         Has_Signature : Boolean := False;
         Value         : Leander.Core.Trees.Tree_Type;
         Signature     : Leander.Types.Trees.Tree_Type;
         Bound_Name    : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Value_Binding_Record,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   type Binding_List is tagged
      record
         Map : Binding_Maps.Map;
      end record;

   function Has_Binding
     (List : Binding_List;
      Name : String)
      return Boolean
   is (List.Map.Contains (Name));

   function Has_Value
     (List : Binding_List;
      Name : String)
      return Boolean
   is (List.Map.Contains (Name)
       and then List.Map.Element (Name).Has_Value);

   function Has_Signature
     (List : Binding_List;
      Name : String)
      return Boolean
   is (List.Map.Contains (Name)
       and then List.Map.Element (Name).Has_Signature);

   function Bound_Name
     (List : Binding_List;
      Name : String)
      return String
   is (Ada.Strings.Unbounded.To_String
       (List.Map.Element (Name).Bound_Name));

   function Binding
     (List : Binding_List;
      Name : String)
      return Leander.Core.Trees.Tree_Type
   is (List.Map.Element (Name).Value);

   function Signature
     (List : Binding_List;
      Name : String)
      return Leander.Types.Trees.Tree_Type
   is (List.Map.Element (Name).Signature);

end Leander.Core.Bindings;
