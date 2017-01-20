private with Ada.Containers.Vectors;

with Leander.Core.Trees;

package Leander.Core.Cases is

   type Case_Builder is tagged limited private;

   procedure Set_Case_Expression
     (Builder    : in out Case_Builder'Class;
      Expression : Leander.Core.Trees.Tree_Type);

   procedure Add_Alt
     (Builder    : in out Case_Builder'Class;
      Pattern    : Leander.Core.Trees.Tree_Type;
      Expression : Leander.Core.Trees.Tree_Type);

   function Transform
     (Builder : Case_Builder'Class)
      return Leander.Core.Trees.Tree_Type;

private

   package Tree_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Core.Trees.Tree_Type, Leander.Core.Trees."=");

   type Case_Builder is tagged limited
      record
         Case_Expr : Leander.Core.Trees.Tree_Type;
         Pats      : Tree_Vectors.Vector;
         Exps      : Tree_Vectors.Vector;
      end record;

   function Trivial_Case
     (Builder : Case_Builder'Class)
      return Boolean;
   --  A case consisting of one pattern, which is a single variable

   function Trivial_Constructor_Case
     (Builder : Case_Builder'Class)
      return Boolean;
   --  case K x1 .. xN of K x1 .. xN -> e
   --     => e

   function Simple_Constructor_Case
     (Builder : Case_Builder'Class)
      return Boolean;
   --  case K x1 .. xN of K y1 .. yN -> e
   --     => (\y1.\y2...\yN.e) x1 x2 ... xN

   function Simple_Algebraic_Case
     (Builder : Case_Builder'Class)
      return Boolean;
   --  A case where all constructors have only variables, or a case
   --  where only one top-level constructor is used, and at most one
   --  column does not consist of variables, and each variable in a column
   --  has the same name, and the non-variable column is also simple.

   function Transform_Simple_Algebraic_Case
     (Builder : Case_Builder'Class)
      return Leander.Core.Trees.Tree_Type;

   function Transform_General_Algebraic_Case
     (Builder : Case_Builder'Class)
      return Leander.Core.Trees.Tree_Type;

end Leander.Core.Cases;
