with Leander.Allocator;

package body Leander.Core.Schemes is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("schemes", Instance, Variable_Reference);

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Allocator.Allocate (Instance (This))));

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance
   is
      W : Leander.Names.Name_Array (This.Tyvars'Range);
   begin
      for I in W'Range loop
         W (I) := Leander.Names.Leander_Name (This.Tyvars (I).Name);
      end loop;
      return Quantify (This.Tyvars, This.Ty.Apply (Subst.Without (W)));
   end Apply;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean
   is
      use type Leander.Core.Tyvars.Instance;
   begin
      for Tv of This.Get_Tyvars loop
         if Leander.Core.Tyvars.Instance (Tyvar) = Tv then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --------------------
   -- Fresh_Instance --
   --------------------

   function Fresh_Instance
     (This : Instance)
      return Types.Reference
   is
      function Create_Subst
        (Index : Positive)
         return Leander.Core.Substitutions.Instance;

      ------------------
      -- Create_Subst --
      ------------------

      function Create_Subst
        (Index : Positive)
         return Leander.Core.Substitutions.Instance
      is
      begin
         if Index <= This.Count then
            return Leander.Core.Substitutions.Compose
              (Leander.Names.Leander_Name (This.Tyvars (Index).Name),
               Leander.Core.Types.New_TVar,
               Create_Subst (Index + 1));
         else
            return Leander.Core.Substitutions.Empty;
         end if;
      end Create_Subst;

   begin
      return This.Ty.Apply (Create_Subst (1));
   end Fresh_Instance;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array
   is
      use type Leander.Core.Tyvars.Tyvar_Array;
   begin
      return This.Ty.Get_Tyvars / This.Tyvars;
   end Get_Tyvars;


   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

   --------------
   -- Quantify --
   --------------

   function Quantify
     (Vs    : Leander.Core.Tyvars.Tyvar_Array;
      T     : Leander.Core.Types.Reference)
      return Reference
   is
   begin
      return Allocate (Instance'(Vs'Length, Vs, T));
   end Quantify;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
   begin
      return This.Ty.Show;
   end Show;

   ---------------
   -- To_Scheme --
   ---------------

   function To_Scheme
     (T     : Leander.Core.Types.Reference)
      return Reference
   is
   begin
      return Quantify ([], T);
   end To_Scheme;

end Leander.Core.Schemes;
