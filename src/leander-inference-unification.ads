with Leander.Core.Substitutions;

package Leander.Inference.Unification is

   procedure Unify
     (T1, T2 : Leander.Core.Types.Reference;
      Subst  : in out Leander.Core.Substitutions.Reference);

end Leander.Inference.Unification;
