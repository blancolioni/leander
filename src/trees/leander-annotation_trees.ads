with Leander.Trees;
with Leander.Unifiable;

generic
   type Node_Type is
     new Show_Interface
     and Leander.Unifiable.Unifiable_Interface with private;
   type Annotation_Type is
     new Leander.Trees.Tree_Interface
     and Leander.Unifiable.Unifiable_Interface with private;
   with function Has_Annotation (Node : Node_Type'Class) return Boolean;
   with function Annotation (Node : Node_Type'Class) return Annotation_Type;
package Leander.Annotation_Trees is

   type Tree_Type is
     new Leander.Trees.Tree_Interface
     and Leander.Unifiable.Unifiable_Interface
   with private;

   subtype Tree_Class is Tree_Type'Class;

   overriding function Apply (Left, Right : Tree_Type) return Tree_Type;

   overriding function "=" (Left, Right : Tree_Type) return Boolean;

   overriding function Is_Empty (Tree : Tree_Type) return Boolean;

   overriding function Is_Leaf (Tree : Tree_Type) return Boolean;
   overriding function Is_Application (Tree : Tree_Type) return Boolean;

   function Get_Node (Tree : Tree_Type) return Node_Type
     with Pre => Is_Leaf (Tree);

   function Head (Tree : Tree_Type) return Node_Type;
   function Arity (Tree : Tree_Type) return Natural;

   overriding function Left (Tree : Tree_Type) return Tree_Type;

   overriding function Right (Tree : Tree_Type) return Tree_Type;

   overriding function Empty return Tree_Type;

   function Leaf (Node : Node_Type) return Tree_Type;
   function Apply (Left, Right : Node_Type) return Tree_Type;
   function Apply (Left : Node_Type;
                   Right : Tree_Type)
                   return Tree_Type;

   function Apply (Left  : Tree_Type;
                   Right : Node_Type)
                   return Tree_Type;

   overriding function Show (Tree : Tree_Type) return String;
   function Show_With_Annotations (Tree : Tree_Type) return String;

   function Has_Annotation (Tree : Tree_Type) return Boolean;
   function Annotation (Tree : Tree_Type) return Annotation_Type
     with Pre => Tree.Has_Annotation;

   procedure Set_Annotation
     (Tree       : Tree_Type;
      Annotation : Annotation_Type);

private

   type Tree_Node (Leaf : Boolean);

   type Tree_Node_Access is access Tree_Node;

   type Tree_Type is
     new Leander.Trees.Tree_Interface
     and Leander.Unifiable.Unifiable_Interface with
      record
         Node : Tree_Node_Access;
      end record;

   procedure Replace_Node
     (Tree : Tree_Type'Class;
      Node : Node_Type);

   type Tree_Node (Leaf : Boolean) is
      record
         Has_Annotation : Boolean := False;
         Annotation     : Annotation_Type;
         case Leaf is
            when True =>
               Node : Node_Type;
            when False =>
               Left, Right : Tree_Type;
         end case;
      end record;

   overriding function Is_Binding
     (Tree : Tree_Type)
      return Boolean
   is (Tree.Is_Leaf and then Tree.Node.Node.Is_Binding);

   overriding function Is_Variable
     (Tree : Tree_Type)
      return Boolean
   is (Tree.Is_Leaf and then Tree.Node.Node.Is_Variable);

   overriding function Is_Constructor
     (Tree : Tree_Type)
      return Boolean
   is (Tree.Is_Leaf and then Tree.Node.Node.Is_Constructor);

   overriding function Binding_Index
     (Tree : Tree_Type)
      return Positive
   is (Tree.Node.Node.Binding_Index);

   overriding function Variable_Name
     (Tree : Tree_Type)
      return String
   is (Tree.Node.Node.Variable_Name);

   overriding function Constructor_Name
     (Tree : Tree_Type)
      return String
   is (Tree.Node.Node.Constructor_Name);

   overriding procedure Set_Binding_Index
     (Tree   : in out Tree_Type;
      Index  : Positive);

   overriding function Create_Variable_From_Binding
     (Tree  : Tree_Type;
      Index : Positive)
      return Tree_Type
   is (Leaf (Tree.Node.Node.Create_Variable_From_Binding (Index)));

   overriding function "=" (Left, Right : Tree_Type) return Boolean
   is ((Left.Is_Empty and then Right.Is_Empty)
       or else (not Left.Is_Empty
                and then not Right.Is_Empty
                and then Left.Is_Leaf
                and then Right.Is_Leaf
                and then Left.Node.Node = Right.Node.Node));

   overriding function Is_Empty (Tree : Tree_Type) return Boolean
   is (Tree.Node = null);

   overriding function Is_Leaf (Tree : Tree_Type) return Boolean
   is (Tree.Node /= null and then Tree.Node.Leaf);

   overriding function Is_Application (Tree : Tree_Type) return Boolean
   is (Tree.Node /= null and then not Tree.Node.Leaf);

   function Get_Node (Tree : Tree_Type) return Node_Type
   is (Tree.Node.Node);

   overriding function Left (Tree : Tree_Type) return Tree_Type
   is (Tree.Node.Left);

   overriding function Right (Tree : Tree_Type) return Tree_Type
   is (Tree.Node.Right);

   overriding function Empty return Tree_Type
   is (Node => null);

   function Leaf (Node : Node_Type) return Tree_Type
   is (Node => new Tree_Node'(Leaf   => True,
                              Node   => Node,
                              others => <>));

   overriding function Apply (Left, Right : Tree_Type) return Tree_Type
   is (Node => new Tree_Node'(Leaf           => False,
                              Has_Annotation => False,
                              Left           => Left,
                              Right          => Right,
                              others         => <>));

   function Apply (Left, Right : Node_Type) return Tree_Type
   is (Apply (Leaf (Left), Leaf (Right)));

   function Apply (Left  : Node_Type;
                   Right : Tree_Type)
                   return Tree_Type
   is (Apply (Leaf (Left), Right));

   function Apply (Left  : Tree_Type;
                   Right : Node_Type)
                   return Tree_Type
   is (Apply (Left, Leaf (Right)));

   function Has_Annotation (Tree : Tree_Type) return Boolean
   is (not Tree.Is_Empty
       and then (Tree.Node.Has_Annotation
                 or else (Tree.Is_Leaf
                          and then Has_Annotation (Tree.Node.Node))));

   function Annotation (Tree : Tree_Type) return Annotation_Type
   is (if Tree.Node.Has_Annotation
       then Tree.Node.Annotation
       else Annotation (Tree.Node.Node));

   function Head (Tree : Tree_Type) return Node_Type
   is (if Tree.Is_Leaf
       then Tree.Get_Node
       else Tree.Left.Head);

   function Arity (Tree : Tree_Type) return Natural
   is (if Tree.Is_Leaf
       then 0
       else 1 + Arity (Tree.Left));

end Leander.Annotation_Trees;
