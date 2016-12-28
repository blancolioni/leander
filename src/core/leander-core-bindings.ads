private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Strings.Fixed.Hash;

with Leander.Core.Trees;

package Leander.Core.Bindings is

   type Binding_List is tagged private;

   function Has_Binding
     (List : Binding_List;
      Name : String)
      return Boolean;

   function Binding
     (List : Binding_List;
      Name : String)
      return Leander.Core.Trees.Tree_Type
     with Pre => List.Has_Binding (Name);

   procedure Insert
     (List    : in out Binding_List;
      Name    : String;
      Binding : Leander.Core.Trees.Tree_Type)
     with Pre => not List.Has_Binding (Name),
     Post => List.Has_Binding (Name);

   procedure Scan
     (List    : Binding_List;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type));

private

   package Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Leander.Core.Trees.Tree_Type,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => Leander.Core.Trees."=");

   type Binding_List is tagged
      record
         Map : Binding_Maps.Map;
      end record;

   function Has_Binding
     (List : Binding_List;
      Name : String)
      return Boolean
   is (List.Map.Contains (Name));

   function Binding
     (List : Binding_List;
      Name : String)
      return Leander.Core.Trees.Tree_Type
   is (List.Map.Element (Name));

end Leander.Core.Bindings;
