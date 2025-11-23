package body Leander.Core.Qualifiers is

   function Allocate
     (This : Instance)
      return Reference
   is (Reference (Core.Allocate (This)));

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance
   is
      Ps : Predicate_Lists.List;
   begin
      for P of This.Predicates loop
         Ps.Append
           (Core.Predicates.Predicate
              (P.Class_Name,
               P.Get_Type.Apply (Subst)));
      end loop;
      return Allocate (Instance'(Predicates => Ps));
   end Apply;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array
   is
      function Get
        (Position : Predicate_Lists.Cursor)
         return Leander.Core.Tyvars.Tyvar_Array;

      function Get
        (Position : Predicate_Lists.Cursor)
         return Leander.Core.Tyvars.Tyvar_Array
      is
      begin
         if not Predicate_Lists.Has_Element (Position) then
            return [];
         else
            return Core.Tyvars.Union
              (Predicate_Lists.Element (Position).Get_Type.Get_Tyvars,
               Get (Predicate_Lists.Next (Position)));
         end if;
      end Get;

   begin
      return Get (This.Predicates.First);
   end Get_Tyvars;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (This : not null access constant Instance'Class;
      Refs : Leander.Core.Types.Type_Array)
      return Reference
   is
   begin
      return Allocate (Instance'
                         (Predicates => [for P of This.Predicates =>
                                             Core.Predicates.Predicate
                                           (P.Class_Name,
                                            P.Get_Type.Instantiate (Refs))]));
   end Instantiate;

   -------------
   -- Qualify --
   -------------

   function Qualify
     (Predicates : Leander.Core.Predicates.Predicate_Array)
      return Reference
   is
   begin
      return Allocate (Instance'(Predicates => [for P of Predicates => P]));
   end Qualify;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is

      function Shw (Position : Predicate_Lists.Cursor) return String;

      ---------
      -- Shw --
      ---------

      function Shw (Position : Predicate_Lists.Cursor) return String is
         use Predicate_Lists;
      begin
         if not Has_Element (Next (Position)) then
            return Element (Position).Show;
         else
            return Element (Position).Show & "," & Shw (Next (Position));
         end if;
      end Shw;

   begin
      case Natural (This.Predicates.Length) is
         when 0 =>
            return "";
         when 1 =>
            return This.Predicates.First_Element.Show;
         when others =>
            return "(" & Shw (This.Predicates.First) & ")";
      end case;
   end Show;

end Leander.Core.Qualifiers;
