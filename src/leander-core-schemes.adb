with Leander.Allocator;
with Leander.Core.Qualifiers;

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
      Result : constant Variable_Reference := Allocator.Allocate
        (Instance'
           (Count => This.count,
            Ks    => This.Ks,
            QT    => This.QT.Apply (Subst)));
   begin
      return Result;
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
      return Qualified_Types.Reference
   is
      function New_Tyvar
        (K : Leander.Core.Kinds.Kind)
         return Leander.Core.Types.Reference
      is (Leander.Core.Types.TVar
            (Leander.Core.Tyvars.New_Tyvar (K)));

      Ts : constant Core.Types.Type_Array :=
             [for K of This.Ks => New_Tyvar (K)];
   begin
      return This.QT.Instantiate (Ts);
   end Fresh_Instance;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array
   is
   begin
      return This.QT.Get_Tyvars;
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
      T     : not null access constant Qualified_Types.Instance'Class)
      return Reference
   is
      Tvs : constant Leander.Core.Tyvars.Tyvar_Array :=
              Tyvars.Intersection
                (T.Get_Tyvars, Vs);
      Ks  : constant Kind_Array :=
              [for V of Tvs => V.Get_Kind];
      S   : Leander.Core.Substitutions.Instance :=
              Core.Substitutions.Empty;
      G   : Natural := 1;
   begin
      for V of Tvs loop
         S := Substitutions.Compose (Leander.Names.Leander_Name (V.Name),
                                     Types.TGen (G), S);
         G := G + 1;
      end loop;
      return Allocate
        (Instance'
           (Ks'Length, Ks, T.Apply (S)));
   end Quantify;

   --------------
   -- Quantify --
   --------------

   function Quantify
     (Vs    : Leander.Core.Tyvars.Tyvar_Array;
      Ps    : Leander.Core.Predicates.Predicate_Array;
      T     : Leander.Core.Types.Reference)
      return Reference
   is
   begin
      return Quantify (Vs, Qualified_Types.Qualified_Type (Ps, T));
   end Quantify;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
   begin
      return This.QT.Show;
   end Show;

   ---------------
   -- To_Scheme --
   ---------------

   function To_Scheme
     (T     : Leander.Core.Types.Reference)
      return Reference
   is
   begin
      return Quantify
        ([],
         Qualified_Types.Qualified_Type
           (Qualifiers.Empty, T));
   end To_Scheme;

end Leander.Core.Schemes;
