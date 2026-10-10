with Leander.Core.Alts.Inference;
with Leander.Core.Expressions;
with Leander.Core.Type_Classes;
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
         Base      : constant Natural := Context.Predicate_Count;

         --  The signature's type variables, freshly instantiated.  The
         --  body may not refine them: see Too_General below.
         Declared  : constant Core.Tyvars.Tyvar_Array := T.Get_Tyvars;

         procedure Report (Message : String);
         --  Report Message at the binding.  The context is left alone: the
         --  declared signature still stands for every use of the binding,
         --  so the rest of the module can be checked against it, and
         --  failing the context would only bury this report under the
         --  ones that failure provokes everywhere else.

         ------------
         -- Report --
         ------------

         procedure Report (Message : String) is
            Body_Expr : constant Core.Expressions.Reference :=
                          Explicit.Alts (Explicit.Alts'First).Expression;
         begin
            Body_Expr.Error (Message);
            Context.Reject;
         end Report;

      begin
         Infer_Alts (Explicit.Alts, T);

         declare
            use type Core.Tyvars.Tyvar_Array;
            use type Core.Inference.Class_Environment_Reference;
            Subst : constant Substitutions.Instance :=
                      Context.Current_Substitution;
            Q1 : constant Core.Qualifiers.Reference := Q.Apply (Subst);
            T1 : constant Core.Types.Reference := T.Apply (Subst);
            Fs : constant Core.Tyvars.Tyvar_Array :=
                   Start_Env.Apply (Subst).all.Get_Tyvars;
            Gs : constant Core.Tyvars.Tyvar_Array :=
                   T1.Get_Tyvars / Fs;
            Signature : constant String :=
                          To_String (Explicit.Name)
                          & " :: " & Explicit.Scheme.Show;

            function Too_General return Boolean;
            --  True when the body fixes one of the signature's type
            --  variables: binds it to a type, to a variable of the
            --  enclosing scope, or to another of them (Jones 1999,
            --  section 11.6.2: sc /= sc').

            -----------------
            -- Too_General --
            -----------------

            function Too_General return Boolean is
               Images : constant Core.Types.Type_Array
                 (1 .. Declared'Length) :=
                   [for I in 1 .. Declared'Length =>
                      Core.Types.TVar
                        (Declared (Declared'First + I - 1)).Apply (Subst)];
            begin
               for I in Images'Range loop
                  if not Images (I).Is_Variable then
                     return True;
                  end if;

                  declare
                     V : constant Core.Tyvars.Instance :=
                           Images (I).Variable;
                  begin
                     if Core.Tyvars.Intersection ([V], Fs)'Length > 0
                       or else
                         (for some J in Images'First .. I - 1 =>
                            Images (J).Variable.Name = V.Name)
                     then
                        return True;
                     end if;
                  end;
               end loop;
               return False;
            end Too_General;

         begin
            --  Only a context that knows the classes checks signatures.
            --  Class defaults and instance methods are inferred in their
            --  own contexts, against the class's method schemes, and are
            --  not checked here.
            if Context.Class_Environment = null then
               null;
            elsif Too_General then
               Report
                 ("type signature too general: " & Signature
                  & ", but its body has type "
                  & Schemes.Quantify
                      (Gs, Qualified_Types.Qualified_Type (Q1, T1)).Show);
            else
               --  Every predicate the body raises over the signature's own
               --  type variables has to follow from the declared context
               --  (section 11.6.2 again: rs must be empty).  The rest are
               --  about the enclosing scope, and are deferred to it.
               declare
                  Raised  : constant Core.Predicates.Predicate_Array :=
                              Context.Current_Predicates;
                  Missing : Core.Predicates.Predicate_Array
                    (1 .. Raised'Last - Base);
                  Count   : Natural := 0;
               begin
                  for K in Base + 1 .. Raised'Last loop
                     if Core.Tyvars.Intersection
                       (Raised (K).Get_Type.Get_Tyvars, Gs)'Length > 0
                       and then not Context.Class_Environment.Entails
                         (Q1.Predicates, Raised (K))
                       and then
                         (for all J in 1 .. Count =>
                            Missing (J).Show /= Raised (K).Show)
                     then
                        Count := Count + 1;
                        Missing (Count) := Raised (K);
                     end if;
                  end loop;

                  if Count > 0 then
                     declare
                        use type Core.Predicates.Predicate_Array;
                     begin
                        Report
                          ("context too weak: " & Signature
                           & ", but its body has type "
                           & Schemes.Quantify
                               (Gs,
                                Qualified_Types.Qualified_Type
                                  (Q1.Predicates & Missing (1 .. Count),
                                   T1)).Show);
                     end;
                        end if;
               end;
            end if;

            --  The declared context, as this body's own type variables:
            --  a use site applies one dictionary per predicate, in scheme
            --  order, and a local binding is elaborated with a lambda for
            --  each (see the ELet case of Expressions.To_Calculus).
            Explicit.Set_Dictionaries (Q1.Predicates);
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

            --  The same predicates, still attached to the context's type
            --  variables.  A monomorphic binding defers all of its
            --  predicates outwards, and the variable one of them
            --  constrains is often not resolved until the binding's single
            --  use site is inferred -- which, for a nested group, happens
            --  after this point.  Putting back the substituted copies from
            --  All_Ps would freeze them as they stand now and leave the
            --  enclosing scope holding a dictionary for a type variable
            --  that nothing ever resolves.
            Raw_Ps : constant Core.Predicates.Predicate_Array :=
                       Context.Raw_Predicates;

            function Over_Gs (P : Core.Predicates.Instance) return Boolean
            is (Core.Tyvars.Intersection
                  (P.Get_Type.Apply (Subst).all.Get_Tyvars, Gs)'Length > 0);
            --  True when P constrains a variable this group quantifies, so
            --  the predicate must travel with the scheme rather than be
            --  deferred to whatever encloses the group.

            function Select_Preds
              (From, To : Positive;
               Want     : Boolean)
               return Core.Predicates.Predicate_Array;
            --  The predicates bindings From .. To raised, without
            --  duplicates: those over Gs if Want, the others if not.

            function Select_Preds
              (I    : Positive;
               Want : Boolean)
               return Core.Predicates.Predicate_Array
            is (Select_Preds (I, I, Want));

            ------------------
            -- Select_Preds --
            ------------------

            function Select_Preds
              (From, To : Positive;
               Want     : Boolean)
               return Core.Predicates.Predicate_Array
            is
               use type Core.Predicates.Instance;
               Result : Core.Predicates.Predicate_Array
                 (1 .. Base (To + 1) - Base (From));
               Last   : Natural := 0;
            begin
               for K in Base (From) + 1 .. Base (To + 1) loop
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
                  --
                  --  The bindings of a group share one context (Jones 1999,
                  --  section 11.6.2): a group mate's recursive use is
                  --  monomorphic and applies no dictionaries, so the whole
                  --  group must be elaborated under the same ones (see the
                  --  ELet case of Expressions.To_Calculus).
                  declare
                     Ps : constant Core.Predicates.Predicate_Array :=
                            Select_Preds (Bs'First, Bs'Last, Want => True);
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
                       (Raw_Ps (Base (I) + 1 .. Base (I + 1)));
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
