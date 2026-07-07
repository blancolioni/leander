with Leander.Names;
with Skit.Terms;

package Leander.Calculus is

   type Tree is private;

   function Lambda
     (Name : String;
      Expr : Tree)
      return Tree;

   function Lambda
     (Name : Leander.Names.Leander_Name;
      Expr : Tree)
      return Tree;

   function Lambda
     (Index : Natural;
      Expr  : Tree)
      return Tree;

   function Apply
     (Left, Right : Tree)
      return Tree;

   function Number
     (Value : Integer)
      return Tree;

   function Symbol
     (Name : String)
      return Tree;

   function Symbol
     (Name : Leander.Names.Leander_Name)
      return Tree;

   function Symbol
     (Index : Natural)
      return Tree;

   function Compile
     (This     : Tree)
      return Skit.Terms.Term;

   procedure Dispose (This : in out Tree);

   function Has_Reference
     (This : Tree;
      Name : String)
      return Boolean;

   function To_String (This : Tree) return String;

private

   type Node_Record;

   type Tree is access Node_Record;

end Leander.Calculus;
