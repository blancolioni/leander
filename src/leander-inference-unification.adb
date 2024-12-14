with Leander.Core.Tyvars;

package body Leander.Inference.Unification is

   function Unify
     (T1, T2 : Leander.Core.Types.Reference;
      Subst  : Leander.Inference.Substitutions.Reference)
      return Leander.Inference.Substitutions.Reference;

   -----------
   -- Unify --
   -----------

   function Unify
     (T1, T2 : Leander.Core.Types.Reference;
      Subst  : Leander.Inference.Substitutions.Reference)
      return Leander.Inference.Substitutions.Reference
   is
      use type Core.Name_Id;

      function Bind_Variable
        (Tyvar : Core.Tyvars.Reference;
         To    : Core.Types.Reference)
         return Substitutions.Reference;

      -------------------
      -- Bind_Variable --
      -------------------

      function Bind_Variable
        (Tyvar : Core.Tyvars.Reference;
         To    : Core.Types.Reference)
         return Substitutions.Reference
      is
      begin
         if To.Is_Variable and then To.Variable.Name = Tyvar.Name then
            return Substitutions.Empty;
         elsif To.Contains (Tyvar) then
            raise Constraint_Error with "occurs check failed";
         else
            return Substitutions.Substitute (Tyvar, To);
         end if;
      end Bind_Variable;

   begin
      if T1.Is_Application then
         if T2.Is_Application then
            declare
               S1 : constant Substitutions.Reference :=
                      Unify (T1.Left, T2.Left, Subst);
               S2 : constant Substitutions.Reference :=
                      Unify (S1.Apply (T1.Right), S1.Apply (T2.Right), Subst);
            begin
               return S2.Merge (S1);
            end;
         end if;
      elsif T1.Is_Variable then
         return Bind_Variable (T1.Variable, T2);
      elsif T2.Is_Variable then
         return Bind_Variable (T2.Variable, T1);
      elsif T1.Is_Constructor then
         if T2.Is_Constructor then
            if T1.Constructor.Name = T2.Constructor.Name then
               return Substitutions.Empty;
            end if;
         end if;
      end if;
      raise Constraint_Error with
        "unification failure: "
        & T1.Show
        & " <-> "
        & T2.Show;
   end Unify;

   -----------
   -- Unify --
   -----------

   procedure Unify
     (T1, T2 : Leander.Core.Types.Reference;
      Subst  : in out Leander.Inference.Substitutions.Reference)
   is
   begin
      Subst := Unify (Subst.Apply (T1), Subst.Apply (T2), Subst).Merge (Subst);
   end Unify;

end Leander.Inference.Unification;
