with Leander.Core.Alts.Inference;
with Leander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Core.Qualifiers;
with Leander.Core.Schemes;
with Leander.Core.Substitutions;
with Leander.Core.Type_Env;
with Leander.Core.Types.Unification;
with Leander.Core.Tyvars;

package body Leander.Core.Binding_Groups.Inference is

   -----------
   -- Infer --
   -----------

   procedure Infer
     (Context       : in out Leander.Core.Inference.Inference_Context'Class;
      Binding_Group : not null access constant Instance'Class)
   is
      procedure Infer_Explicit_Binding
        (Explicit : Leander.Core.Bindings.Reference);

      procedure Infer_Implicit_Bindings
        (Bs : Leander.Core.Bindings.Reference_Array);

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
            Leander.Core.Types.Unification.Unify
              (Context, T, Context.Binding (Alt));
         end loop;
      end Infer_Alts;

      ----------------------------
      -- Infer_Explicit_Binding --
      ----------------------------

      procedure Infer_Explicit_Binding
        (Explicit : Leander.Core.Bindings.Reference)
      is
         QT : constant Core.Qualified_Types.Reference :=
                Explicit.Scheme.Fresh_Instance;
         Q  : constant Core.Qualifiers.Reference := QT.Qualifier;
         T  : constant Core.Types.Reference := QT.Get_Type;
         Start_Env : constant Core.Type_Env.Reference := Context.Type_Env;
      begin
         Infer_Alts (Explicit.Alts, T);

         declare
            use type Core.Tyvars.Tyvar_Array;
            Subst : constant Substitutions.Instance :=
                      Context.Current_Substitution;
            Q1 : constant Core.Qualifiers.Reference := Q.Apply (Subst);
            T1 : constant Core.Types.Reference := T.Apply (Subst);
            Fs : constant Core.Tyvars.Tyvar_Array :=
                   Start_Env.Apply (Subst).all.Get_Tyvars;
            Gs : constant Core.Tyvars.Tyvar_Array :=
                   T1.Get_Tyvars / Fs;
            Sc1 : constant Leander.Core.Schemes.Reference :=
                    Schemes.Quantify
                      (Gs, Qualified_Types.Qualified_Type (Q1, T1));
         begin
            pragma Unreferenced (Sc1);
         end;
      exception
         when others =>
            Context.Error
              ("inference failed: "
               & To_String (Explicit.Name)
               & " :: " & Explicit.Scheme.Show);
      end Infer_Explicit_Binding;

      -----------------------------
      -- Infer_Implicit_Bindings --
      -----------------------------

      procedure Infer_Implicit_Bindings
        (Bs : Leander.Core.Bindings.Reference_Array)
      is
         Ts : Core.Types.Type_Array (Bs'Range) :=
                [others => Core.Types.New_TVar];
         Scs : Core.Schemes.Reference_Array (Ts'Range) :=
                 [for T of Ts => Core.Schemes.To_Scheme (T)];
         Ids : constant array (Bs'Range) of Varid :=
                 [for B of Bs => B.Name];
         Start_Env : constant Core.Type_Env.Reference := Context.Type_Env;
         Env       : Core.Type_Env.Reference := Start_Env;
         Subst     : Core.Substitutions.Instance := Core.Substitutions.Empty;

         --  Base (I) is how many predicates the context held before binding
         --  I was inferred, so the ones it raised itself are the slice that
         --  follows.  Base (Bs'Last + 1) closes the last slice.
         Base : array (Bs'First .. Bs'Last + 1) of Natural := [others => 0];
      begin
         for I in Ids'Range loop
            Env := Env.Compose (Ids (I), Scs (I));
         end loop;
         Context.Update_Type_Env (Env);
         Env := Start_Env;
         for I in Bs'Range loop
            Base (I) := Context.Predicate_Count;
            Infer_Alts (Bs (I).Alts, Ts (I));
            declare
               S1 : constant Core.Substitutions.Instance'Class :=
                      Core.Types.Unification.Most_General_Unifier
                        (Ts (I), Context.Binding (Bs (I).Alts (1)));
            begin
               Subst := Context.Current_Substitution.Compose
                 (Core.Substitutions.Instance (S1).Compose (Subst));
            end;

            Env := Env.Compose
              (Ids (I),
               Core.Schemes.To_Scheme
                 (Context.Binding (Bs (I).Alts (1).Expression)));
         end loop;
         Base (Bs'Last + 1) := Context.Predicate_Count;
         for I in Ts'Range loop
            Ts (I) := Ts (I).Apply (Subst);
         end loop;

         declare
            use type Core.Tyvars.Tyvar_Array;
            Fs : constant Core.Tyvars.Tyvar_Array :=
                   Start_Env.Apply (Subst).all.Get_Tyvars;
            function New_Tyvars
              (Index : Positive)
               return Core.Tyvars.Tyvar_Array
            is (if Index <= Ts'Last
                then Core.Tyvars.Union
                  (Ts (Index).all.Get_Tyvars,
                   New_Tyvars (Index + 1))
                else []);

            Gs : constant Core.Tyvars.Tyvar_Array :=
                   New_Tyvars (Ts'First) / Fs;

            All_Ps : constant Core.Predicates.Predicate_Array :=
                       Context.Current_Predicates;

            function Over_Gs (P : Core.Predicates.Instance) return Boolean
            is (Core.Tyvars.Intersection
                  (P.Get_Type.Apply (Subst).all.Get_Tyvars, Gs)'Length > 0);
            --  True when P constrains a variable this group quantifies, so
            --  the predicate must travel with the scheme rather than be
            --  deferred to whatever encloses the group.

            function Select_Preds
              (I    : Positive;
               Want : Boolean)
               return Core.Predicates.Predicate_Array;

            ------------------
            -- Select_Preds --
            ------------------

            function Select_Preds
              (I    : Positive;
               Want : Boolean)
               return Core.Predicates.Predicate_Array
            is
               use type Core.Predicates.Instance;
               Result : Core.Predicates.Predicate_Array
                 (1 .. Base (I + 1) - Base (I));
               Last   : Natural := 0;
            begin
               for K in Base (I) + 1 .. Base (I + 1) loop
                  if Over_Gs (All_Ps (K)) = Want
                    and then (for all J in 1 .. Last =>
                                Result (J) /= All_Ps (K))
                  then
                     Last := Last + 1;
                     Result (Last) := All_Ps (K);
                  end if;
               end loop;
               return Result (1 .. Last);
            end Select_Preds;

         begin
            for I in Ts'Range loop
               --  A monomorphic binding keeps its type variables free, so
               --  they stay shared with its single use site.  Quantifying it
               --  would freshen them there instead, stranding any class
               --  constraint raised in the body on a type variable nothing
               --  ever resolves (issue #70).
               if Bs (I).Monomorphic then
                  Scs (I) := Core.Schemes.To_Scheme (Ts (I));
               else
                  --  Predicates over the variables this binding quantifies
                  --  travel with its scheme, so a use site instantiates them
                  --  and applies one dictionary each; the binding is
                  --  elaborated with one dictionary lambda per predicate, in
                  --  that same order.  The rest defer outwards.
                  declare
                     Ps : constant Core.Predicates.Predicate_Array :=
                            Select_Preds (I, Want => True);
                  begin
                     Scs (I) := Core.Schemes.Quantify (Gs, Ps, Ts (I));
                     Bs (I).Set_Dictionaries (Ps);
                  end;
               end if;
            end loop;

            --  Rewrite the context's predicate list so the group leaves only
            --  what it could not discharge: a retained predicate is now the
            --  binding's own dictionary parameter, not the enclosing scope's.
            if Bs'Length > 0 then
               Context.Drop_Predicates (Base (Bs'First) + 1);
               for I in Bs'Range loop
                  if not Bs (I).Monomorphic then
                     Context.Save_Predicates (Select_Preds (I, Want => False));
                  else
                     Context.Save_Predicates
                       (All_Ps (Base (I) + 1 .. Base (I + 1)));
                  end if;
               end loop;
            end if;
         end;

         Env := Start_Env;
         for I in Ids'Range loop
            Env := Env.Compose (Ids (I), Scs (I));
         end loop;

         Context.Update_Type_Env (Env);
      end Infer_Implicit_Bindings;

   begin

      if not Binding_Group.Explicit_Bindings.Is_Empty then
         declare
            Env : Core.Type_Env.Reference := Context.Type_Env;
         begin
            for B of Binding_Group.Explicit_Bindings.First_Element loop
               Env := Env.Compose (B.Name, B.Scheme);
            end loop;
            Context.Update_Type_Env (Env);
         end;
      end if;

      for Bs of Binding_Group.Implicit_Bindings loop
         Infer_Implicit_Bindings (Bs);
      end loop;

      if not Binding_Group.Explicit_Bindings.Is_Empty then
         for B of Binding_Group.Explicit_Bindings.First_Element loop
            Infer_Explicit_Binding (B);
         end loop;
      end if;

   end Infer;

end Leander.Core.Binding_Groups.Inference;
