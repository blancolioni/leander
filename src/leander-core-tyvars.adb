with Leander.Core.Types;

package body Leander.Core.Tyvars is

   Next_Tyvar : Positive := 1;

   function Contains (Tvs : Tyvar_Array;
                      Tv  : Instance'Class)
                      return Boolean;

   ---------
   -- "/" --
   ---------

   function "/"
     (Container : Tyvar_Array;
      Tvs       : Tyvar_Array)
      return Tyvar_Array
   is
      function Get (Index : Positive) return Tyvar_Array
      is (if Index > Container'Last
          then []
          elsif Contains (Tvs, Container (Index))
          then Get (Index + 1)
          else Container (Index) & Get (Index + 1));

   begin
      return Get (Container'First);
   end "/";

   --------------
   -- Contains --
   --------------

   function Contains (Tvs : Tyvar_Array;
                      Tv  : Instance'Class)
                      return Boolean
   is
   begin
      for Item of Tvs loop
         if Item.Name = Tv.Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   function Generate
     (This : not null access constant Container_Abstraction'Class)
      return access constant Container_Abstraction'Class
   is
      Tvs  : constant Core.Tyvars.Tyvar_Array := This.Get_Tyvars;
      Gens : constant Core.Types.Type_Array :=
               [for I in Tvs'Range => Core.Types.TGen (I)];

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
         if Index <= Tvs'Length then
            return Leander.Core.Substitutions.Compose
              (Leander.Names.Leander_Name (Tvs (Index).Name),
               Gens (Index),
               Create_Subst (Index + 1));
         else
            return Leander.Core.Substitutions.Empty;
         end if;
      end Create_Subst;

   begin
      return This.Apply (Create_Subst (1));
   end Generate;

   -------------
   -- Include --
   -------------

   procedure Include (This : in out Tyvar_Array_Builder;
                      Tvs  : Tyvar_Array)
   is
   begin
      for Tv of Tvs loop
         This.Include (Tv);
      end loop;
   end Include;

   -------------
   -- Include --
   -------------

   procedure Include (This : in out Tyvar_Array_Builder;
                      Tv   : Instance'Class)
   is
   begin
      if not This.Vector.Contains (Instance (Tv)) then
         This.Vector.Append (Instance (Tv));
      end if;
   end Include;

   ------------------
   -- Intersection --
   ------------------

   function Intersection
     (X, Y : Tyvar_Array)
      return Tyvar_Array
   is
      function Get (Index : Positive) return Tyvar_Array
      is (if Index > X'Last
          then []
          elsif not Contains (Y, X (Index))
          then Get (Index + 1)
          else X (Index) & Get (Index + 1));

   begin
      return Get (X'First);
   end Intersection;

   ---------------
   -- New_Tyvar --
   ---------------

   function New_Tyvar
     (Kind : Leander.Core.Kinds.Kind := Leander.Core.Kinds.Star)
      return Instance is
      Name : String := Next_Tyvar'Image;
   begin
      Next_Tyvar := Next_Tyvar + 1;
      Name (Name'First) := '_';
      return Tyvar (To_Varid ("$t" & Name), Kind);
   end New_Tyvar;

   ---------
   -- Nub --
   ---------

   function Nub
     (Tvs : Tyvar_Array)
      return Tyvar_Array
   is
      function Get (Acc   : Tyvar_Array;
                    Index : Positive)
                    return Tyvar_Array
      is (if Index > Tvs'Last
          then Acc
          elsif Contains (Acc, Tvs (Index))
          then Get (Acc, Index + 1)
          else Get (Acc & Tvs (Index), Index + 1));
   begin
      return Get ([], Tvs'First);
   end Nub;

   --------------------
   -- To_Tyvar_Array --
   --------------------

   function To_Tyvar_Array
     (This : Tyvar_Array_Builder)
      return Tyvar_Array
   is
   begin
      return [for Tv of This.Vector => Tv];
   end To_Tyvar_Array;

   -----------
   -- Tyvar --
   -----------

   function Tyvar
     (Id   : Varid;
      Kind : Leander.Core.Kinds.Kind)
      return Instance
   is
   begin
      return Instance'(Id, Kind);
   end Tyvar;

   -----------
   -- Union --
   -----------

   function Union
     (X, Y : Tyvar_Array)
      return Tyvar_Array
   is
   begin
      return Nub (X & Y);
   end Union;

end Leander.Core.Tyvars;
