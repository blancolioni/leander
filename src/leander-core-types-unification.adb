with Leander.Logging;

package body Leander.Core.Types.Unification is

   function Bind_Variable
     (Name    : Varid;
      Binding : Reference)
      return Leander.Core.Substitutions.Instance;

   procedure Unification_Failure
     (Message     : String;
      Left, Right : String)
     with No_Return;

   -------------------
   -- Bind_Variable --
   -------------------

   function Bind_Variable
     (Name    : Varid;
      Binding : Reference)
      return Leander.Core.Substitutions.Instance
   is
   begin
      if Binding.Tag = TVar and then Binding.Tyvar.Name = Name then
         return Leander.Core.Substitutions.Empty;
      end if;

      for Tv of Binding.Get_Tyvars loop
         if Tv.Name = Name then
            Unification_Failure
              ("occurs check", To_String (Name), Binding.Show);
         end if;
      end loop;
      return Leander.Core.Substitutions.Compose
        (Leander.Names.Leander_Name (Name), Binding,
         Leander.Core.Substitutions.Empty);
   end Bind_Variable;

   --------------------------
   -- Most_General_Unifier --
   --------------------------

   function Most_General_Unifier
     (Left, Right : Reference)
      return Leander.Core.Substitutions.Instance
   is
   begin
      Leander.Logging.Log
        ("MGU",
         Left.Show & " <> " & Right.Show);
      if Left.Tag = TVar then
         return Bind_Variable (Left.Tyvar.Name, Right);
      elsif Right.Tag = TVar then
         return Bind_Variable (Right.Tyvar.Name, Left);
      elsif Left.Tag /= Right.Tag then
         raise Unification_Error with
         Left.Show & " vs " & Right.Show;
      else
         case Left.Tag is
            when TVar =>
               raise Program_Error with "never happens";
            when TCon =>
               if Left.Tycon.Id = Right.Tycon.Id then
                  return Leander.Core.Substitutions.Empty;
               else
                  Unification_Failure ("types do not unify",
                                       Left.Show, Right.Show);
               end if;
            when TApp =>
               declare
                  S1 : constant Leander.Core.Substitutions.Instance :=
                         Most_General_Unifier (Left.Left, Right.Left);
                  S2 : constant Leander.Core.Substitutions.Instance :=
                         Most_General_Unifier
                           (Left.Right.Apply (S1),
                            Right.Right.Apply (S1));
               begin
                  return S1.Compose (S2);
               end;
            when TGen =>
               return Leander.Core.Substitutions.Empty;
         end case;
      end if;
   end Most_General_Unifier;

   -----------
   -- Unify --
   -----------

   procedure Unify
     (Context     : in out Leander.Core.Inference.Inference_Context'Class;
      Left, Right : Reference)
   is
      U : constant Leander.Core.Substitutions.Instance :=
            Most_General_Unifier
              (Left.Apply (Context.Current_Substitution),
               Right.Apply (Context.Current_Substitution));
   begin
      Context.Save_Substitution (U);
   end Unify;

   -------------------------
   -- Unification_Failure --
   -------------------------

   procedure Unification_Failure
     (Message     : String;
      Left, Right : String)
   is
   begin
      raise Unification_Error with
        Message & ": " & Left & " <-> " & Right;
   end Unification_Failure;

end Leander.Core.Types.Unification;
