with Leander.Core.Bindings;
with Leander.Core.Literals;
with Leander.Core.Schemes;
with Leander.Core.Substitutions;
with Leander.Core.Tyvars;

with Leander.Inference.Unification;

with Leander.Logging;

package body Leander.Inference is

   package Type_Subst renames Leander.Core.Substitutions;

   type Inference_Visitor is
     new Leander.Core.Expressions.Expression_Visitor with
      record
         Assumptions   : Leander.Core.Assumptions.Reference;
         Substitutions : Type_Subst.Reference :=
                           Type_Subst.Empty;
         Result        : Leander.Core.Types.Reference :=
                           Leander.Core.Types.T_Error;
      end record;

   overriding procedure Variable
     (This : in out Inference_Visitor;
      Id   : Core.Name_Id);

   overriding procedure Constructor
     (This   : in out Inference_Visitor;
      Id     : Core.Name_Id;
      C_Type : Core.Types.Reference);

   overriding procedure Literal
     (This    : in out Inference_Visitor;
      Literal : Core.Literals.Reference);

   overriding procedure Application
     (This    : in out Inference_Visitor;
      F, X    : Core.Expressions.Reference);

   overriding procedure Lambda
     (This    : in out Inference_Visitor;
      X       : Core.Name_Id;
      E       : Core.Expressions.Reference);

   overriding procedure Let
     (This       : in out Inference_Visitor;
      Binding    : Core.Bindings.Reference;
      Expression : Core.Expressions.Reference);

   function Infer_Type
     (This        : in out Inference_Visitor'Class;
      Assumptions : Leander.Core.Assumptions.Reference;
      Expression  : Leander.Core.Expressions.Reference)
      return Leander.Core.Types.Reference;

   procedure Unify
     (This   : in out Inference_Visitor'Class;
      T1, T2 : Core.Types.Reference);

   function Infer_Binding_Type
     (Binding     : Core.Bindings.Reference;
      Assumptions : Core.Assumptions.Reference;
      Subst       : Type_Subst.Reference)
      return Core.Assumptions.Reference;

   -----------------
   -- Application --
   -----------------

   overriding procedure Application
     (This    : in out Inference_Visitor;
      F, X    : Core.Expressions.Reference)
   is
      TF : constant Core.Types.Reference :=
             This.Infer_Type (This.Assumptions, F);
      TX : constant Core.Types.Reference :=
             This.Infer_Type (This.Assumptions, X);
      T  : constant Core.Types.Reference :=
             Core.Types.New_TVar;
   begin
      This.Unify (TX.Fn (T), TF);
      This.Result := T;
   end Application;

   -----------------
   -- Constructor --
   -----------------

   overriding procedure Constructor
     (This   : in out Inference_Visitor;
      Id     : Core.Name_Id;
      C_Type : Core.Types.Reference)
   is
   begin
      This.Result := C_Type;
   end Constructor;

   ------------------------
   -- Infer_Binding_Type --
   ------------------------

   function Infer_Binding_Type
     (Binding     : Core.Bindings.Reference;
      Assumptions : Core.Assumptions.Reference;
      Subst       : Type_Subst.Reference)
      return Core.Assumptions.Reference
   is
      use type Core.Tyvars.Tyvar_Array;
      T1 : constant Core.Types.Reference :=
             Infer_Expression_Type
               (Assumptions, Binding.Binding);
      T2 : constant Core.Types.Reference :=
             T1.Apply (Subst);
      Tvs : constant Core.Tyvars.Tyvar_Array :=
              T2.Get_Tyvars / Assumptions.Apply (Subst).Get_Tyvars;
      Sc  : constant Core.Schemes.Reference :=
              Core.Schemes.Quantify (Tvs, T2);
   begin
      return Core.Assumptions.Assumption (Binding.Id, Sc);
   end Infer_Binding_Type;

   ---------------------------
   -- Infer_Expression_Type --
   ---------------------------

   function Infer_Expression_Type
     (Assumptions : Leander.Core.Assumptions.Reference;
      Expression  : Leander.Core.Expressions.Reference)
      return Leander.Core.Types.Reference
   is
      This : Inference_Visitor :=
               (Assumptions => Assumptions,
                others      => <>);
   begin
      This.Assumptions := Assumptions;
      Expression.Visit (This);
      Leander.Logging.Log
        ("INFERENCE",
         This.Substitutions.Show);
      Leander.Logging.Log
        ("INFERENCE",
         This.Result.Show);
      return This.Result.Apply (This.Substitutions);
   end Infer_Expression_Type;

   ----------------
   -- Infer_Type --
   ----------------

   function Infer_Type
     (This        : in out Inference_Visitor'Class;
      Assumptions : Leander.Core.Assumptions.Reference;
      Expression  : Leander.Core.Expressions.Reference)
      return Leander.Core.Types.Reference
   is
      Old_Assumptions : constant Core.Assumptions.Reference :=
                          This.Assumptions;
   begin
      This.Assumptions := Assumptions;
      Expression.Visit (This);
      This.Assumptions := Old_Assumptions;
      Leander.Logging.Log ("INFERENCE",
                           Expression.Show
                           & " :: "
                           & This.Result.Show);
      return This.Result;
   end Infer_Type;

   ------------
   -- Lambda --
   ------------

   overriding procedure Lambda
     (This    : in out Inference_Visitor;
      X       : Core.Name_Id;
      E       : Core.Expressions.Reference)
   is
      T : constant Core.Types.Reference := Core.Types.New_TVar;
      Scheme : constant Core.Schemes.Reference :=
                 Core.Schemes.To_Scheme (T);
      TE : constant Core.Types.Reference :=
             This.Infer_Type (This.Assumptions.Prepend (X, Scheme), E);
   begin
      This.Result := T.Fn (TE);
   end Lambda;

   ---------
   -- Let --
   ---------

   overriding procedure Let
     (This       : in out Inference_Visitor;
      Binding    : Core.Bindings.Reference;
      Expression : Core.Expressions.Reference)
   is
      Assumptions : constant Core.Assumptions.Reference :=
                      Infer_Binding_Type
                        (Binding, This.Assumptions, This.Substitutions);
   begin
      This.Result :=
        This.Infer_Type
          (This.Assumptions.Prepend (Assumptions),
           Expression);
   end Let;

   -------------
   -- Literal --
   -------------

   overriding procedure Literal
     (This    : in out Inference_Visitor;
      Literal : Core.Literals.Reference)
   is
   begin
      This.Result := Literal.Literal_Type;
   end Literal;

   -----------
   -- Unify --
   -----------

   procedure Unify
     (This   : in out Inference_Visitor'Class;
      T1, T2 : Core.Types.Reference)
   is
   begin
      Leander.Logging.Log
        ("UNIFY-LEFT", T1.Show);
      Leander.Logging.Log
        ("UNIFY-RIGHT", T2.Show);
      Leander.Logging.Log
        ("SUB-1", This.Substitutions.Show);
      Unification.Unify (T1, T2, This.Substitutions);
      Leander.Logging.Log
        ("SUB", This.Substitutions.Show);
   end Unify;

   --------------
   -- Variable --
   --------------

   overriding procedure Variable
     (This : in out Inference_Visitor;
      Id   : Core.Name_Id)
   is
      Scheme : constant Core.Assumptions.Maybe_Schemes.Maybe :=
            This.Assumptions.Find (Id);
   begin
      if Scheme.Is_Nothing then
         raise Constraint_Error with
           "undefined: " & Leander.Core.Show (Id);
      else
         This.Result := Scheme.From_Just.Fresh_Instance;
      end if;
   end Variable;

end Leander.Inference;
