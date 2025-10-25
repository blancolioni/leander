with Leander.Core.Kinds;
with Leander.Core.Types;
with Leander.Core.Tycons;
with Leander.Core.Tyvars;
with Leander.Core.Type_Env;

package body Leander.Environment.Prelude is

   ------------
   -- Create --
   ------------

   function Create return Reference is
      use Leander.Core.Type_Env, Leander.Core.Schemes;
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
   begin
      Env.Data_Type
        (Tycon => T_Bool,
         Kind => Core.Kinds.Star,
         Cons => Compose (Empty, "True", To_Scheme (T_Bool))
         .Compose ("False", To_Scheme (T_Bool)));
      Env.Data_Type
        (Tycon => Core.Types.T_List,
         Kind => Core.Kinds.Kind_Function (Core.Kinds.Star, Core.Kinds.Star),
         Cons =>
           Empty
         .Compose ("[]", Quantify ([Tv_A], T_List_A))
         .Compose (":",
           Quantify
             ([Tv_A],
              Core.Types.Fn (T_A,
                Core.Types.Fn (T_List_A, T_List_A)))));

      return Env;
   end Create;

end Leander.Environment.Prelude;
