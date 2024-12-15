package body Leander.Core.Schemes is

   type Instance is new Abstraction with
      record
         Count : Natural;
         T     : Types.Reference;
      end record;

   overriding function Show
     (This  : Instance)
      return String;

   overriding function Apply
     (This  : Instance;
      Subst : Substitutions.Reference)
      return Reference;

   overriding function Contains
     (This  : Instance;
      Tyvar : Tyvars.Reference)
      return Boolean;

   overriding function Fresh_Instance
     (This : Instance)
      return Types.Reference;

   overriding function Get_Tyvars
     (This  : Instance)
      return Tyvars.Tyvar_Array;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : Instance;
      Subst : Substitutions.Reference)
      return Reference
   is
      App : constant Instance := (This.Count, This.T.Apply (Subst));
   begin
      return new Instance'(App);
   end Apply;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (This  : Instance;
      Tyvar : Tyvars.Reference)
      return Boolean
   is
   begin
      return This.T.Contains (Tyvar);
   end Contains;

   --------------------
   -- Fresh_Instance --
   --------------------

   overriding function Fresh_Instance
     (This : Instance)
      return Types.Reference
   is
      use type Types.Reference_Array;

      function New_Tyvars (Count : Natural) return Types.Reference_Array
      is (if Count = 0
          then []
          else Types.New_TVar & New_Tyvars (Count - 1));

      Ts : Types.Reference_Array := New_Tyvars (This.Count);
   begin
      for T of Ts loop
         T := Types.New_TVar;
      end loop;
      return This.T.Instantiate (Ts);
   end Fresh_Instance;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Tyvars.Tyvar_Array
   is
   begin
      return This.T.Get_Tyvars;
   end Get_Tyvars;

   --------------
   -- Quantify --
   --------------

   function Quantify
     (Vs    : Tyvars.Tyvar_Array;
      T     : Types.Reference)
      return Reference
   is
      use type Substitutions.Type_Reference_Array;

      T_Vs : constant Tyvars.Tyvar_Array := T.Get_Tyvars;
      New_Vs : constant Tyvars.Tyvar_Array :=
                 Tyvars.Intersection (T_Vs, Vs);

      function New_TGens (Count : Natural)
                          return Substitutions.Type_Reference_Array
      is (if Count = 0 then []
          else New_TGens (Count - 1)
          & Substitutions.Type_Reference (Types.TGen (Count)));

      Subst : constant Substitutions.Reference :=
                Substitutions.Substitute
                  (New_Vs, New_TGens (New_Vs'Length));
      Q : constant Instance := (New_Vs'Length, T.Apply (Subst));
   begin
      return new Instance'(Q);
   end Quantify;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This  : Instance)
      return String
   is
      Img : constant String := This.Count'Image;
   begin
      return "<" & Img (2 .. Img'Last) & ">" & This.T.Show;
   end Show;

   ---------------
   -- To_Scheme --
   ---------------

   function To_Scheme
     (T     : Types.Reference)
      return Reference
   is
      Sc : constant Instance := (0, T);
   begin
      return new Instance'(Sc);
   end To_Scheme;

end Leander.Core.Schemes;
