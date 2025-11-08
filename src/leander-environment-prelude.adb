with Leander.Core.Kinds;
with Leander.Core.Types;
with Leander.Core.Tycons;
with Leander.Core.Tyvars;
with Leander.Data_Types.Builder;

package body Leander.Environment.Prelude is

   ------------
   -- Create --
   ------------

   function Create return Reference is
      use Leander.Core.Schemes;
      Env    : constant Reference := New_Environment ("Prelude");
      Tv_A   : constant Core.Tyvars.Instance :=
                 Core.Tyvars.Tyvar (Core.To_Varid ("a"), Core.Kinds.Star);
      T_A    : constant Core.Types.Reference :=
                 Core.Types.TVar (Tv_A);
      T_Bool : constant Core.Types.Reference :=
                 Core.Types.TCon
                   (Core.Tycons.Tycon
                      (Core.To_Conid ("Bool"),
                       Core.Kinds.Star));
      T_List_A : constant Core.Types.Reference :=
                   Core.Types.List_Of (T_A);
      Builder  : Leander.Data_Types.Builder.Data_Type_Builder;
   begin
      Builder.Start
        (Core.Types.T_Unit);
      Builder.Add_Con
        (Core.To_Conid ("()"), To_Scheme (Core.Types.T_Unit));
      Builder.Build;
      Env.Data_Type (Builder.Data_Type);

      Builder.Start
        (T_Bool);
      Builder.Add_Con
        (Core.To_Conid ("True"), To_Scheme (T_Bool));
      Builder.Add_Con
        (Core.To_Conid ("False"), To_Scheme (T_Bool));
      Builder.Build;
      Env.Data_Type (Builder.Data_Type);

      Builder.Start
        (T_List_A);
      Builder.Add_Con
        (Core.To_Conid ("[]"), Quantify ([Tv_A], T_List_A));
      Builder.Add_Con
        (Core.To_Conid (":"),
         Quantify
           ([Tv_A],
            Core.Types.Fn (T_A,
              Core.Types.Fn (T_List_A, T_List_A))));
      Builder.Build;
      Env.Data_Type (Builder.Data_Type);

      return Env;
   end Create;

end Leander.Environment.Prelude;
