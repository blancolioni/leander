package body Leander.Core.Tyvars is

   type Instance is new Abstraction with
      record
         Id    : Name_Id;
         Kind  : Kinds.Reference;
      end record;

   overriding function Kind (This : Instance) return Kinds.Reference
   is (This.Kind);

   overriding function Show
     (This : Instance)
      return String
   is (Show (This.Id));

   overriding function Name (This : Instance) return Name_Id
   is (This.Id);

   function Contains
     (Tvs : Tyvar_Array;
      Tv  : Reference)
      return Boolean
   is (for some T of Tvs => T.Name = Tv.Name);

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

   -----------
   -- Tyvar --
   -----------

   function Tyvar
     (Id   : Name_Id;
      Kind : Kinds.Reference)
      return Reference
   is
      This : constant Instance := Instance'(Id, Kind);
   begin
      return new Instance'(This);
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
