with Leander.Types.Trees;

package Leander.Syntax.Expressions is

   function Literal
     (Source       : Leander.Source.Source_Reference;
      Text         : String;
      Literal_Type : Leander.Types.Trees.Tree_Type)
      return Syntax_Tree'Class;

   function Is_Literal (Tree : Syntax_Tree) return Boolean;

   function Variable
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Syntax_Tree;

   function Is_Variable (Tree : Syntax_Tree) return Boolean;

   function Constructor
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Syntax_Tree;

   function Is_Constructor (Tree : Syntax_Tree) return Boolean;

   function Apply
     (Source      : Leander.Source.Source_Reference;
      Left, Right : Syntax_Tree'Class)
      return Syntax_Tree;

   function Lambda
     (Source   : Leander.Source.Source_Reference;
      Variable : String;
      Expr     : Syntax_Tree'Class)
      return Syntax_Tree;

   function Case_Expression
     (Source     : Leander.Source.Source_Reference;
      Expression : Syntax_Tree'Class)
      return Syntax_Tree;

   procedure Add_Case_Alternate
     (Case_Expr : Syntax_Tree'Class;
      Pattern    : Syntax_Tree'Class;
      Expression : Syntax_Tree'Class);

   function Name
     (Tree : Syntax_Tree)
      return String
     with Pre => Is_Constructor (Tree)
     or else Is_Literal (Tree)
     or else Is_Variable (Tree);

   function Application_Name (Tree : Syntax_Tree) return String
   is (Name (Tree.Left_Most));

   function Application_Arguments
     (Tree : Syntax_Tree)
      return Array_Of_Syntax_Trees;

end Leander.Syntax.Expressions;
