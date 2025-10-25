with Leander.Names;
with Skit.Machine;

package Leander.Calculus is

   type Tree is private;

   function Lambda
     (Name : String;
      Expr : Tree)
      return Tree;

   function Lambda
     (Name : Leander.Names.leander_Name;
      Expr : Tree)
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
     (Name : Leander.Names.leander_Name)
      return Tree;

   procedure Compile
     (This    : Tree;
      Machine : not null access Skit.Machine.Abstraction'Class);

   procedure Dispose (This : in out Tree);

private

   type Node_Record;

   type Tree is access Node_Record;

end Leander.Calculus;
