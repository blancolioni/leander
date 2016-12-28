with Leander.Primitives;

package body Leander.Prelude is

   --------------------
   -- Load_Built_Ins --
   --------------------

   procedure Load_Built_Ins
     (Env : in out Leander.Environments.Environment)
   is
   begin
      Env.Declare_Primitive_Type
        ("Int", Leander.Primitives.Int_Type);
      Env.Declare_Data_Type
        ("[]", Leander.Primitives.List_Type);
      Env.Insert_Constructor
        ("[]", "[]", Leander.Primitives.Empty_List);
      Env.Insert_Constructor
        ("[]", ":", Leander.Primitives.Cons);
   end Load_Built_Ins;

end Leander.Prelude;
