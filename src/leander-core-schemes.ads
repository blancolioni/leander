with Leander.Core.Kinds;
with Leander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Core.Substitutions;
with Leander.Core.Types;
with Leander.Core.Tyvars;
with Leander.Showable;

package Leander.Core.Schemes is

   type Instance (<>) is
     new Leander.Core.Tyvars.Container_Abstraction
     and Leander.Showable.Abstraction
   with private;

   type Reference is not null access constant Instance'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function Fresh_Instance
     (This : Instance)
      return Qualified_Types.Reference;

   function Inner_Type
     (This : Instance)
      return Types.Reference;

   function To_Scheme
     (T     : Leander.Core.Types.Reference)
      return Reference;

   function Quantify
     (Vs    : Leander.Core.Tyvars.Tyvar_Array;
      T     : not null access constant Qualified_Types.Instance'Class)
      return Reference;

   function Quantify
     (Vs    : Leander.Core.Tyvars.Tyvar_Array;
      Ps    : Leander.Core.Predicates.Predicate_Array;
      T     : Leander.Core.Types.Reference)
      return Reference;

   overriding function Show (This : Instance) return String;

   procedure Prune;

   procedure Report;

private

   type Kind_Array is array (Positive range <>) of Leander.Core.Kinds.Kind;

   function From_Parts
     (Ks : Kind_Array;
      QT : Qualified_Types.Reference)
      return Reference;
   --  Construct a Scheme directly from its already-quantified parts: Ks are
   --  the kinds of the (implicit) quantified variables in binding order, and
   --  QT's inner type already refers to them via TGen at the matching
   --  indices. Unlike Quantify, this performs no generalization/substitution
   --  -- it exists for Leander.Core.Schemes.Serialize, which reconstructs a
   --  previously-quantified Scheme from decoded bytes and so already has a
   --  QT whose TGen indices are correct by construction.

   type Instance (Count : Natural) is
     new Leander.Core.Tyvars.Container_Abstraction
     and Leander.Showable.Abstraction with
      record
         Ks : Kind_Array (1 .. Count);
         QT : Leander.Core.Qualified_Types.Reference;
      end record;

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean;

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array;

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance;

   function Inner_Type
     (This : Instance)
      return Types.Reference
   is (This.QT.Get_Type);

end Leander.Core.Schemes;
