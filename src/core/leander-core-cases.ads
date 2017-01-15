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

end Leander.Core.Cases;
