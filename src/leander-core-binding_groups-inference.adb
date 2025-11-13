with Ada.Text_IO;

with Leander.Core.Alts.Inference;
with Leander.Core.Qualified_Types;
with Leander.Core.Qualifiers;
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
                   Start_Env.Apply (Subst).Get_Tyvars;
            Gs : constant Core.Tyvars.Tyvar_Array :=
                   T1.Get_Tyvars / Fs;
            Sc1 : constant Leander.Core.Schemes.Reference :=
                    Schemes.Quantify
                      (Gs, Qualified_Types.Qualified_Type (Q1, T1));
         begin
            Leander.Logging.Log
              ("explicit: " & Explicit.Scheme.Show);
            Leander.Logging.Log
              ("Inferred: " & Sc1.Show);
         end;
      exception
         when others =>
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error,
               "inference failed: "
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
               Scs (I) :=
                 Core.Schemes.Quantify (Gs, [], Ts (I));
            end loop;
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
