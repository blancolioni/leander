with Leander.Core.Predicates;
with Leander.Core.Qualifiers;
private with Leander.Core.Substitutions;
with Leander.Core.Tyvars;
with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Qualified_Types is

   type Instance is
     new Leander.Core.Abstraction
     and Leander.Showable.Abstraction
     and Leander.Core.Tyvars.Container_Abstraction
   with private;

   type Reference is access constant Instance'Class;

   function Predicates
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array;

   function Qualifier
     (This : Instance'Class)
      return Leander.Core.Qualifiers.Reference;

   function Get_Type
     (This : Instance'Class)
      return Leander.Core.Types.Reference;

   function Instantiate
     (This : not null access constant Instance'Class;
      Refs : Leander.Core.Types.Type_Array)
      return Reference;

   function Qualified_Type
     (Qualifier : Leander.Core.Qualifiers.Reference;
      QT        : Leander.Core.Types.Reference)
     return Reference;

   function Qualified_Type
     (Ps : Leander.Core.Predicates.Predicate_Array;
      QT : Leander.Core.Types.Reference)
      return Reference;

private

   type Instance is
     new Leander.Core.Abstraction
     and Leander.Showable.Abstraction
     and Leander.Core.Tyvars.Container_Abstraction with
      record
         Qualifier : Leander.Core.Qualifiers.Reference;
         QT        : Leander.Core.Types.Reference;
      end record;

   overriding function Show (This : Instance) return String;

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean;

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array
   is (Core.Tyvars.Union
       (This.Qualifier.Get_Tyvars, This.QT.Get_Tyvars));

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance;

   function Get_Type
     (This : Instance'Class)
      return Leander.Core.Types.Reference
   is (This.QT);

   function Predicates
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array
   is (This.Qualifier.Predicates);

   function Qualifier
     (This : Instance'Class)
      return Leander.Core.Qualifiers.Reference
   is (This.Qualifier);

   function Qualified_Type
     (Ps : Leander.Core.Predicates.Predicate_Array;
      QT : Leander.Core.Types.Reference)
      return Reference
   is (Qualified_Type (Qualifiers.Qualify (Ps), QT));

end Leander.Core.Qualified_Types;
