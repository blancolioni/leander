with Leander.Types.Trees;

package Leander.Syntax.Expressions is

   function Literal
     (Source       : Leander.Source.Source_Reference;
      Text         : String;
      Literal_Type : Leander.Types.Trees.Tree_Type)
      return Syntax_Tree'Class;

   function Variable
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Syntax_Tree;

   function Constructor
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Syntax_Tree;

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

end Leander.Syntax.Expressions;
