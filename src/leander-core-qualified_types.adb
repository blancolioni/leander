package body Leander.Core.Qualified_Types is

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance)
      return Reference
   is
   begin
      return Reference (Core.Allocate (This));
   end Allocate;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance
   is
   begin
      return Qualified_Type
        (This.Qualifier.Apply (Subst), This.QT.Apply (Subst));
   end Apply;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean
   is
   begin
      return This.Qualifier.Contains (Tyvar)
        or else This.QT.Contains (Tyvar);
   end Contains;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (This : not null access constant Instance'Class;
      Refs : Leander.Core.Types.Type_Array)
      return Reference
   is
      Result : constant Instance :=
                 Instance'
                   (Qualifier => This.Qualifier.Instantiate (Refs),
                    QT        => This.QT.Instantiate (Refs));
   begin
      return Allocate (Result);
   end Instantiate;

   --------------------
   -- Qualified_Type --
   --------------------

   function Qualified_Type
     (Qualifier : Leander.Core.Qualifiers.Reference;
      QT        : Leander.Core.Types.Reference)
      return Reference
   is
   begin
      return Allocate (Instance'(Qualifier, QT));
   end Qualified_Type;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
   begin
      return This.Qualifier.SHow & " => " & This.QT.Show;
   end Show;

end Leander.Core.Qualified_Types;
