with Leander.Core.Alts.Inference;
with Leander.Core.Schemes;
with Leander.Core.Substitutions;
with Leander.Core.Type_Env;
with Leander.Core.Types.Unification;
with Leander.Core.Tyvars;
with Leander.Logging;

package body Leander.Core.Binding_Groups.Inference is

   -----------
   -- Infer --
   -----------

   procedure Infer
     (Context       : in out Leander.Core.Inference.Inference_Context'Class;
      Binding_Group : not null access constant Instance'Class)
   is
      procedure Infer_Bindings (Bs : Leander.Core.Bindings.Reference_Array);

      procedure Infer_Alts
        (Alts : Leander.Core.Alts.Reference_Array;
         T    : Leander.Core.Types.Reference);

      ----------------
      -- Infer_Alts --
      ----------------

      procedure Infer_Alts
        (Alts : Leander.Core.Alts.Reference_Array;
         T    : Leander.Core.Types.Reference)
      is
      begin
         for Alt of Alts loop
            Leander.Core.Alts.Inference.Infer (Context, Alt);
         end loop;
         for Alt of Alts loop
            declare
               Subst : constant Leander.Core.Substitutions.Instance :=
                         Leander.Core.Types.Unification.Most_General_Unifier
                           (T, Context.Binding (Alt));
            begin
               Leander.Logging.Log
                 ("INFER-ALT", Alt.Show & " ==> " & Subst.Show);
               pragma Unreferenced (Subst);
            end;
         end loop;
      end Infer_Alts;

      --------------------
      -- Infer_Bindings --
      --------------------

      procedure Infer_Bindings (Bs : Leander.Core.Bindings.Reference_Array) is
         Ts : Core.Types.Type_Array (Bs'Range) :=
                [others => Core.Types.New_TVar];
         Scs : Core.Schemes.Reference_Array (Ts'Range) :=
                 [for T of Ts => Core.Schemes.To_Scheme (T)];
         Ids : constant array (Bs'Range) of Varid :=
                 [for B of Bs => B.Name];
         Start_Env : constant Core.Type_Env.Reference := Context.Type_Env;
         Env       : Core.Type_Env.Reference := Start_Env;
         Subst     : Core.Substitutions.Instance := Core.Substitutions.Empty;
      begin
         for I in Ids'Range loop
            Env := Env.Compose (Ids (I), Scs (I));
         end loop;
         Context.Update_Type_Env (Env);
         Env := Start_Env;
         for I in Bs'Range loop
            Infer_Alts (Bs (I).Alts, Ts (I));
            declare
               S1 : constant Core.Substitutions.Instance'Class :=
                      Core.Types.Unification.Most_General_Unifier
                        (Ts (I), Context.Binding (Bs (I).Alts (1)));
            begin
               Subst := Context.Current_Substitution.Compose
                 (Core.Substitutions.Instance (S1).Compose (Subst));
            end;

            Leander.Logging.Log
              ("SUBST",
               Subst.Show);

            Env := Env.Compose
              (Ids (I),
               Core.Schemes.To_Scheme
                 (Context.Binding (Bs (I).Alts (1).Expression)));
         end loop;
         for I in Ts'Range loop
            Ts (I) := Ts (I).Apply (Subst);
            Leander.Logging.Log
              ("INFER",
               To_String (Ids (I))
               & " :: "
               & Ts (I).Show);
         end loop;

         declare
            use type Core.Tyvars.Tyvar_Array;
            Fs : constant Core.Tyvars.Tyvar_Array :=
                   Start_Env.Apply (Subst).Get_Tyvars;
            function New_Tyvars
              (Index : Positive)
               return Core.Tyvars.Tyvar_Array
            is (if Index <= Ts'Last
                then Core.Tyvars.Union
                  (Ts (Index).Get_Tyvars,
                   New_Tyvars (Index + 1))
                else []);

            Gs : constant Core.Tyvars.Tyvar_Array :=
                   New_Tyvars (Ts'First) / Fs;

         begin
            for I in Ts'Range loop
               Scs (I) := Core.Schemes.Quantify (Gs, Ts (I));
            end loop;
         end;

         Env := Start_Env;
         for I in Ids'Range loop
            Env := Env.Compose (Ids (I), Scs (I));
         end loop;

         Context.Update_Type_Env (Env);
      end Infer_Bindings;

   begin
      for Bs of Binding_Group.Implicit_Bindings loop
         Infer_Bindings (Bs);
      end loop;
   end Infer;

end Leander.Core.Binding_Groups.Inference;
