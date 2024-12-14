with Leander.Core.Assumptions;
with Leander.Core.Expressions;
with Leander.Core.Types;

package Leander.Inference is

   function Infer_Expression_Type
     (Assumptions : Leander.Core.Assumptions.Reference;
      Expression  : Leander.Core.Expressions.Reference)
     return Leander.Core.Types.Reference;

end Leander.Inference;
