with Leander.Core.Trees;
with Leander.Environments;
with Leander.Source;

package Leander.Syntax is

   type Syntax_Tree_Record is tagged private;

   subtype Syntax_Tree is Syntax_Tree_Record'Class;

   function Show (Syntax : Syntax_Tree) return String;

   function Has_Children (Tree : Syntax_Tree) return Boolean;

   function Left_Most (Tree : Syntax_Tree) return Syntax_Tree;

   function To_Core
     (Syntax : Syntax_Tree)
      return Leander.Core.Trees.Tree_Type;

   type Array_Of_Syntax_Trees is
     array (Positive range <>) of Syntax_Tree_Record;

   function Empty_Tree_Array return Array_Of_Syntax_Trees;

private

   type Node_Record is abstract tagged
      record
         Source : Leander.Source.Source_Reference;
      end record;

   function Show (Node : Node_Record) return String is abstract;

   function Has_Left (Node : Node_Record) return Boolean is (False);

   type Node_Access is access all Node_Record'Class;

   type Syntax_Tree_Record is tagged
      record
         Node : Node_Access;
      end record;

   function Create
     (Node : Node_Record'Class)
      return Syntax_Tree;

   function Left_Child (Node : Node_Record) return Syntax_Tree
   is (raise Constraint_Error with "no left child")
   with Pre'Class => Node_Record'Class (Node).Has_Left;

   function Is_Expression
     (Tree : Syntax_Tree_Record'Class)
      return Boolean;

   function Left_Most (Tree : Syntax_Tree_Record'Class) return Syntax_Tree
   is (if Tree.Node.Has_Left
       then Tree.Node.Left_Child.Left_Most
       else Tree);

   function Has_Children (Tree : Syntax_Tree) return Boolean
   is (Tree.Node.Has_Left);

   type Expression_Node is
     abstract new Node_Record with
      record
         null;
      end record;

   function Transform
     (Node   : Expression_Node)
      return Leander.Core.Trees.Tree_Type
      is abstract;

   function Show (Syntax : Syntax_Tree) return String
   is (Syntax.Node.Show);

   function Get_Expression
     (Tree : Syntax_Tree_Record'Class)
      return access Expression_Node'Class
   is (Expression_Node'Class (Tree.Node.all)'Access)
   with Pre => Tree.Is_Expression;

   function Is_Expression
     (Tree : Syntax_Tree_Record'Class)
      return Boolean
   is (Tree.Node.all in Expression_Node'Class);

   Empty_Tree_Constant : Array_Of_Syntax_Trees (1 .. 0);

   function Empty_Tree_Array return Array_Of_Syntax_Trees is
     (Empty_Tree_Constant);

end Leander.Syntax;
