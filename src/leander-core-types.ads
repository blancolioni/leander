with Leander.Core.Kinds;
with Leander.Core.Substitutions;
with Leander.Core.Tycons;
with Leander.Core.Tyvars;
with Leander.Showable;

package Leander.Core.Types is

   type Abstraction is interface
     and Leander.Showable.Abstraction
     and Kinds.Has_Kind
     and Tyvars.Container_Abstraction;

   type Reference is not null access constant Abstraction'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function Is_Variable
     (This : Abstraction)
      return Boolean
      is abstract;

   function Variable
     (This : Abstraction)
      return Tyvars.Reference
      is abstract
     with Pre'Class => This.Is_Variable;

   function Is_Constructor
     (This : Abstraction)
      return Boolean
      is abstract;

   function Constructor
     (This : Abstraction)
      return Tycons.Reference
      is abstract
     with Pre'Class => This.Is_Constructor;

   function Is_Application
     (This : Abstraction)
      return Boolean
      is abstract;

   function Left
     (This : Abstraction)
      return Reference
      is abstract
     with Pre'Class => This.Is_Application;

   function Right
     (This : Abstraction)
      return Reference
      is abstract
     with Pre'Class => This.Is_Application;

   function Instantiate
     (This : not null access constant Abstraction;
      Refs : Reference_Array)
      return Reference
      is abstract;

   function Apply
     (This : not null access constant Abstraction;
      Subst : not null access constant Substitutions.Abstraction'Class)
      return Reference
      is abstract;

   function Apply
     (Refs  : Reference_Array;
      Subst : not null access constant Substitutions.Abstraction'Class)
      return Reference_Array;

   function TVar (T : Tyvars.Reference) return Reference;
   function New_TVar return Reference;

   function TCon (T : Tycons.Reference) return Reference;
   function Application
     (Left  : not null access constant Abstraction;
      Right : not null access constant Abstraction'Class)
      return Reference
      is abstract;

   function TGen (Index : Positive) return Reference;

   function T_Error   return Reference;
   function T_Unit    return Reference;
   function T_Char    return Reference;
   function T_Int     return Reference;
   function T_Integer return Reference;
   function T_Float   return Reference;
   function T_Double  return Reference;

   function T_List    return Reference;
   function T_Arrow   return Reference;
   function T_Tuple_2 return Reference;
   function T_String  return Reference;

   function Fn (From : not null access constant Abstraction'Class;
                To   : not null access constant Abstraction'Class)
                return Reference;

   function List
     (This : not null access constant Abstraction'Class)
      return Reference;

   function Pair (A, B : Reference) return Reference;

   type Type_Visitor is interface;

   procedure Visit
     (This    : not null access constant Abstraction;
      Visitor : in out Type_Visitor'class)
   is abstract;

   procedure Variable
     (This  : in out Type_Visitor;
      Tyvar : Tyvars.Reference)
   is null;

   procedure Constructor
     (This  : in out Type_Visitor;
      Tycon : Tycons.Reference)
   is null;

   procedure Application
     (This        : in out Type_Visitor;
      Left, Right : Reference)
   is null;

end Leander.Core.Types;
