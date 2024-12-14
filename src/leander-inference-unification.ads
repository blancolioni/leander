with Leander.Inference.Substitutions;

package Leander.Inference.Unification is

   procedure Unify
     (T1, T2 : Leander.Core.Types.Reference;
      Subst  : in out Leander.Inference.Substitutions.Reference);

end Leander.Inference.Unification;
