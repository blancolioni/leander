with Ada.Strings.Fixed.Hash;
with Ada.Containers.Vectors;
with Ada.Containers.Indefinite_Hashed_Maps;

package body Leander.Unifier is

   package Binding_Vectors is
     new Ada.Containers.Vectors (Positive, Trees.Tree_Type, Trees."=");

   package Binding_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Positive,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   procedure Unify
     (Left_Tree   : Trees.Tree_Type;
      Right_Tree  : Trees.Tree_Type;
      Success     : in out Boolean;
      Bindings    : in out Binding_Vectors.Vector);

   procedure Bind
     (Root          : in out Trees.Tree_Type;
      Bindings      : in out Binding_Vectors.Vector;
      Next_Variable : in out Natural);

   function Dereference
     (Tree     : Trees.Tree_Type;
      Bindings : Binding_Vectors.Vector)
      return Trees.Tree_Type;

   procedure Scan_Variables
     (Root   : in out Trees.Tree_Type;
      Vector : in out Binding_Vectors.Vector;
      Map    : in out Binding_Maps.Map);

   ----------
   -- Bind --
   ----------

   procedure Bind
     (Root          : in out Trees.Tree_Type;
      Bindings      : in out Binding_Vectors.Vector;
      Next_Variable : in out Natural)
   is
      use Trees;
      Tree : constant Trees.Tree_Type := Dereference (Root, Bindings);
   begin
      if Is_Application (Tree) then
         declare
            Left : Trees.Tree_Type := Trees.Get_Left (Tree);
            Right : Trees.Tree_Type := Trees.Get_Right (Tree);
         begin
            Bind (Left, Bindings, Next_Variable);
            Bind (Right, Bindings, Next_Variable);
            Root := Trees.Apply (Left, Right);
         end;
      else
         declare
            Node : constant Node_Type :=
                     Get_Node (Tree);
         begin
            if Node.Is_Anonymous then
               Next_Variable := Next_Variable + 1;
               declare
                  Index    : constant Positive :=
                               Node.Anonymous_Variable_Index;
                  New_Node : Node_Type;
               begin
                  New_Node.Set_Variable_Index (Next_Variable);
                  Root := Trees.Leaf (New_Node);
                  Bindings (Index) := Root;
               end;
            else
               Root := Tree;
            end if;
         end;
      end if;
   end Bind;

   -----------------
   -- Dereference --
   -----------------

   function Dereference
     (Tree     : Trees.Tree_Type;
      Bindings : Binding_Vectors.Vector)
      return Trees.Tree_Type
   is
   begin
      if Trees.Is_Application (Tree) then
         return Tree;
      else
         declare
            Node : constant Node_Type := Trees.Get_Node (Tree);
         begin
            if Node.Is_Anonymous then
               declare
                  use type Trees.Tree_Type;
                  Value : constant Trees.Tree_Type :=
                            Bindings (Node.Anonymous_Variable_Index);
               begin
                  if Value = Tree then
                     return Tree;
                  else
                     return Dereference (Value, Bindings);
                  end if;
               end;
            else
               return Tree;
            end if;
         end;
      end if;
   end Dereference;

   --------------------
   -- Scan_Variables --
   --------------------

   procedure Scan_Variables
     (Root   : in out Trees.Tree_Type;
      Vector : in out Binding_Vectors.Vector;
      Map    : in out Binding_Maps.Map)
   is
      use Trees;
   begin
      if Is_Application (Root) then
         declare
            Left : Tree_Type := Get_Left (Root);
            Right : Tree_Type := Get_Right (Root);
         begin
            Scan_Variables (Left, Vector, Map);
            Scan_Variables (Right, Vector, Map);
            Root := Apply (Left, Right);
         end;
      else
         declare
            Node : Node_Type := Get_Node (Root);
         begin
            if Node.Is_Anonymous then
               null;
            elsif Node.Is_Variable then
               declare
                  Name : constant String := Node.Variable_Name;
               begin
                  if Map.Contains (Name) then
                     Root := Vector.Element (Map.Element (Name));
                  else
                     Node.Set_Anonymous_Variable (Vector.Last_Index + 1);
                     Map.Insert (Name, Vector.Last_Index + 1);
                     Root := Leaf (Node);
                     Vector.Append (Root);
                  end if;
               end;
            end if;
         end;
      end if;
   end Scan_Variables;

   -----------
   -- Unify --
   -----------

   procedure Unify
     (Left_Tree   : Trees.Tree_Type;
      Right_Tree  : Trees.Tree_Type;
      Success     : in out Boolean;
      Bindings    : in out Binding_Vectors.Vector)
   is
      use Trees;
      Left  : constant Trees.Tree_Type := Dereference (Left_Tree, Bindings);
      Right : constant Trees.Tree_Type := Dereference (Right_Tree, Bindings);
   begin
      Success := True;
      if Is_Leaf (Left) then
         declare
            Left_Node : constant Node_Type :=
                          Get_Node (Left);
         begin
            if Is_Leaf (Right) then
               declare
                  Right_Node : constant Node_Type :=
                                 Get_Node (Right);
               begin
                  if Left_Node.Is_Anonymous then
                     Bindings (Left_Node.Anonymous_Variable_Index) := Right;
                  elsif Right_Node.Is_Anonymous then
                     Bindings (Right_Node.Anonymous_Variable_Index) := Left;
                  else
                     Success :=
                       Left_Node.Constructor_Name
                         = Right_Node.Constructor_Name;
                  end if;
               end;
            elsif Left_Node.Is_Anonymous then
               Bindings (Left_Node.Anonymous_Variable_Index) := Right;
            else
               Success := False;
            end if;
         end;
      elsif Is_Leaf (Right) then
         declare
            Right_Node : constant Node_Type :=
                           Get_Node (Right);
         begin
            if Right_Node.Is_Anonymous then
               Bindings (Right_Node.Anonymous_Variable_Index) := Left;
            else
               Success := False;
            end if;
         end;
      else
         Unify (Get_Left (Left), Get_Left (Right), Success, Bindings);
         if Success then
            Unify (Get_Right (Left), Get_Right (Right), Success, Bindings);
         end if;
      end if;

   end Unify;

   -----------
   -- Unify --
   -----------

   procedure Unify
     (Left, Right : Trees.Tree_Type;
      Success     : out Boolean;
      Result      : out Trees.Tree_Type)
   is
      L                   : Trees.Tree_Type := Left;
      R                   : Trees.Tree_Type := Right;
      Left_Map, Right_Map : Binding_Maps.Map;
      Vector              : Binding_Vectors.Vector;
      Next_Variable       : Natural := 0;
   begin
      Success := True;
      Scan_Variables (L, Vector, Left_Map);
      Scan_Variables (R, Vector, Right_Map);
      Unify (L, R, Success, Vector);
      Result := L;
      Bind (Result, Vector, Next_Variable);
   end Unify;

end Leander.Unifier;
