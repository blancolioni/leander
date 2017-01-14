with Leander.Primitives;

package body Leander.Prelude is

   Prelude_Env : Leander.Environments.Environment;
   Created_Prelude : Boolean := False;

   procedure Load_Built_Ins
     (Env : in out Leander.Environments.Environment);

   --------------------
   -- Load_Built_Ins --
   --------------------

   procedure Load_Built_Ins
     (Env : in out Leander.Environments.Environment)
   is
   begin
      Env.Declare_Primitive_Type
        ("Char", Leander.Primitives.Char_Type);
      Env.Declare_Primitive_Type
        ("Int", Leander.Primitives.Int_Type);
      Env.Declare_Data_Type
        ("->", Leander.Primitives.Map_Type);
      Env.Declare_Data_Type
        ("[]", Leander.Primitives.List_Type);
      Env.Insert_Constructor
        ("[]", "[]", Leander.Primitives.Empty_List);
      Env.Insert_Constructor
        ("[]", ":", Leander.Primitives.Cons);
   end Load_Built_Ins;

   -------------------------
   -- Prelude_Environment --
   -------------------------

   function Prelude_Environment
     return Leander.Environments.Environment
   is
   begin
      if not Created_Prelude then
         Leander.Environments.Create (Prelude_Env, "Prelude");
         Load_Built_Ins (Prelude_Env);
         Created_Prelude := True;
      end if;
      return Prelude_Env;
   end Prelude_Environment;

   ---------------
   -- Use_Tuple --
   ---------------

   procedure Use_Tuple (Arity : Positive) is
      Name : constant String := Leander.Primitives.Tuple_Name (Arity);
      Env : Leander.Environments.Environment :=
              Prelude_Environment;
   begin
      if not Env.Has_Type_Constructor_Binding (Name) then
         Env.Declare_Data_Type (Name, Leander.Primitives.Tuple_Type (Arity));
         Env.Insert_Constructor
           (Name, Name, Leander.Primitives.Tuple_Con (Arity));
      end if;
   end Use_Tuple;

end Leander.Prelude;
