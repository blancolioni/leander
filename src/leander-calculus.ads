with Leander.Names;
with Skit;
with Skit.Environment;

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

   type Calculus_Environment is interface;

   function Lookup
     (This : Calculus_Environment;
      Name : Leander.Names.Leander_Name)
      return Tree
      is abstract;

   procedure Compile
     (This     : Tree;
      Env      : not null access constant Calculus_Environment'Class;
      Skit_Env : Skit.Environment.Reference);

   procedure Dispose (This : in out Tree);

   function To_String (This : Tree) return String;

private

   type Node_Record;

   type Tree is access Node_Record;

end Leander.Calculus;
