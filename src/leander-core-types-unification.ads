package Leander.Core.Types.Unification is

   Unification_Error : exception;

   function Most_General_Unifier
     (Left, Right : Reference)
      return Leander.Core.Substitutions.Instance;

end Leander.Core.Types.Unification;
