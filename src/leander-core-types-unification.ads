with Leander.Core.Inference;
package Leander.Core.Types.Unification is

   Unification_Error : exception;

   function Most_General_Unifier
     (Left, Right : Reference)
      return Leander.Core.Substitutions.Instance;

   procedure Unify
     (Context     : in out Leander.Core.Inference.Inference_Context'Class;
      Left, Right : Reference);

end Leander.Core.Types.Unification;
