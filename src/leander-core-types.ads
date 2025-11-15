with Leander.Core.Kinds;
with Leander.Core.Substitutions;
with Leander.Core.Tycons;
with Leander.Core.Tyvars;
with Leander.Disposable;
with Leander.Showable;
with Leander.Traverseable;

package Leander.Core.Types is

   type Instance (<>) is
     new Leander.Showable.Abstraction
     and Kinds.Has_Kind
     and Tyvars.Container_Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Traverseable.Abstraction
   with private;

   type Reference is not null access constant Instance'Class;

   type Type_Array is array (Positive range <>) of Reference;

   function Is_Variable
     (This : Instance)
      return Boolean;

   function Variable
     (This : Instance)
      return Tyvars.Instance
     with Pre'Class => This.Is_Variable;

   function Is_Constructor
     (This : Instance)
      return Boolean;

   function Constructor
     (This : Instance)
      return Tycons.Instance
     with Pre'Class => This.Is_Constructor;

   function Head
     (This : Instance)
      return Conid
     with Pre => not This.Is_Variable;

   function Is_Application
     (This : Instance)
      return Boolean;

   function Left
     (This : Instance)
      return Reference
     with Pre'Class => This.Is_Application;

   function Right
     (This : Instance)
      return Reference
     with Pre'Class => This.Is_Application;

   function Equivalent
     (Left, Right : not null access constant Instance'Class)
      return Boolean;

   function Instantiate
     (This : not null access constant Instance'Class;
      Refs : Type_Array)
      return Reference;

   function TVar (T : Tyvars.Instance) return Reference;

   function New_TVar return Reference;

   function TCon (T : Tycons.Instance) return Reference;
   function Application
     (Left, Right  : not null access constant Instance'Class)
      return Reference;

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
   function T_Pair    return Reference;
   function T_String  return Reference;

   function Fn (From : Reference;
                To   : Reference)
                return Reference;

   function List_Of
     (This : not null access constant Instance'Class)
      return Reference;

   function Pair (A, B : Reference) return Reference;

   procedure Prune;

private

   type Instance_Tag is (TVar, TCon, TGen, TApp);

   type Instance (Tag : Instance_Tag) is
     new Leander.Showable.Abstraction
     and Kinds.Has_Kind
     and Tyvars.Container_Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Traverseable.Abstraction with
      record
         case Tag is
            when TVar =>
               Tyvar : Leander.Core.Tyvars.Instance;
            when TCon =>
               Tycon : Leander.Core.Tycons.Instance;
            when TGen =>
               Index : Positive;
            when TApp =>
               Left, Right : Reference;
         end case;
      end record;

   overriding function Get_Kind
     (This : Instance)
      return Leander.Core.Kinds.Kind;

   overriding function Show (This : Instance) return String;

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

   overriding procedure Dispose (This : in out Instance);

   overriding procedure Traverse
     (This : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Traverseable.Abstraction'Class));

   function Is_Variable
     (This : Instance)
      return Boolean
   is (This.Tag = TVar);

   function Variable
     (This : Instance)
      return Tyvars.Instance
   is (This.Tyvar);

   function Is_Constructor
     (This : Instance)
      return Boolean
   is (This.Tag = TCon);

   function Constructor
     (This : Instance)
      return Tycons.Instance
   is (This.Tycon);

   function Is_Application
     (This : Instance)
      return Boolean
   is (This.Tag = TApp);

   function Left
     (This : Instance)
      return Reference
   is (This.Left);

   function Right
     (This : Instance)
      return Reference
   is (This.Right);

end Leander.Core.Types;
