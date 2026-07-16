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
   begin
      if This.Count = 0 then
         return This;
      end if;
      declare
         New_Ps : constant Core.Predicates.Predicate_Array :=
           [for P of This.Predicates =>
              Core.Predicates.Predicate
                (P.Class_Name,
                 P.Get_Type.all.Apply (Subst))];
      begin
         return Allocate (Instance'(New_Ps'Length, New_Ps));
      end;
   end Apply;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array
   is
      function Get
        (Index : Positive)
         return Leander.Core.Tyvars.Tyvar_Array
      is
        (if Index > This.Count then []
         else Core.Tyvars.Union
           (This.Predicates (Index).Get_Type.all.Get_Tyvars,
            Get (Index + 1)));

   begin
      return Get (1);
   end Get_Tyvars;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (This : not null access constant Instance'Class;
      Refs : Leander.Core.Types.Type_Array)
      return Reference
   is
      New_Ps : constant Core.Predicates.Predicate_Array :=
        [for P of This.Predicates =>
           Core.Predicates.Predicate
             (P.Class_Name,
              P.Get_Type.all.Instantiate (Refs))];
   begin
      return Allocate (Instance'(New_Ps'Length, New_Ps));
   end Instantiate;

   -------------
   -- Qualify --
   -------------

   function Qualify
     (Predicates : Leander.Core.Predicates.Predicate_Array)
      return Reference
   is
   begin
      return Allocate (Instance'(Predicates'Length, Predicates));
   end Qualify;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is

      function Shw (Index : Positive) return String
      is (if Index = This.Count then This.Predicates (Index).Show
          else This.Predicates (Index).Show & "," & Shw (Index + 1));

   begin
      case This.Count is
         when 0 =>
            return "";
         when 1 =>
            return This.Predicates (1).Show;
         when others =>
            return "(" & Shw (1) & ")";
      end case;
   end Show;

end Leander.Core.Qualifiers;
