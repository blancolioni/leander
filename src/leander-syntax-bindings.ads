private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;
private with Ada.Containers.Doubly_Linked_Lists;

with Leander.Core.Binding_Groups;
with Leander.Names;
limited with Leander.Syntax.Expressions;
with Leander.Syntax.Patterns;
with Leander.Core.Predicates;
with Leander.Syntax.Qualified_Types;

package Leander.Syntax.Bindings is

   subtype Parent is Leander.Syntax.Instance;
   type Instance is new Parent with private;
   type Reference is access all Instance'Class;

   type Binding_LHS (<>) is private;

   function Create_Binding_LHS
     (Name      : String;
      Pats      : Patterns.Reference_Array)
      return Binding_LHS;

   function Name (Rec : Binding_LHS) return String;
   function Pats (Rec : Binding_LHS) return Patterns.Reference_Array;

   procedure Add_Binding
     (This        : in out Instance;
      Loc         : Source.Source_Location;
      Name        : String;
      Pats        : Patterns.Reference_Array;
      Expr        : not null access constant Expressions.Instance'Class;
      Fallthrough : String := "");

   procedure Add_Binding
     (This        : in out Instance;
      Loc         : Source.Source_Location;
      Bound       : Binding_LHS;
      Expr        : not null access constant Expressions.Instance'Class;
      Fallthrough : String := "");
   --  Fallthrough names the free variable Expr falls out to when every
   --  guard in it fails, and is empty for an unguarded equation.  It is
   --  left unbound here because what a failed guard should do depends on
   --  what follows in the equation list; Leander.Syntax.Bindings.Guards
   --  binds it once the whole list is known.

   procedure Add_Type
     (This      : in out Instance;
      Loc       : Source.Source_Location;
      Name      : String;
      Type_Expr : Leander.Syntax.Qualified_Types.Reference);

   function To_Core
     (This          : Instance)
      return Leander.Core.Binding_Groups.Reference;

   function Empty
     (Context     : Core.Declaration_Context := Core.Binding_Context;
      Predicates  : Leander.Core.Predicates.Predicate_Array := [];
      Monomorphic : Boolean := False)
      return Reference;
   --  Monomorphic marks a group the desugarer synthesises for a construct
   --  that is not a source-level 'let' -- case alternatives, if-then-else,
   --  a do-block's pattern bind.  Each such binding is applied at exactly
   --  one site, and generalising it strands class constraints raised in its
   --  body (see Leander.Core.Bindings.Monomorphic).


private

   type Binding_LHS (Name_Length, Pat_Count : Natural) is
      record
         Name : String (1 .. Name_Length);
         Pats : Patterns.Reference_Array (1 .. Pat_Count);
      end record;

   function Create_Binding_LHS
     (Name      : String;
      Pats      : Patterns.Reference_Array)
      return Binding_LHS
   is (Name'Length, Pats'Length, Name, Pats);

   function Name (Rec : Binding_LHS) return String is (Rec.Name);
   function Pats (Rec : Binding_LHS) return Patterns.Reference_Array
   is (Rec.Pats);

   type Expression_Reference is
     access constant Leander.Syntax.Expressions.Instance'Class;

   type Binding_Record (Pat_Count : Natural) is
      record
         Pats        : Patterns.Reference_Array (1 .. Pat_Count);
         Expr        : Expression_Reference;
         Fallthrough : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Binding_Record_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists (Binding_Record);

   type Name_Binding is
      record
         Name      : Leander.Names.Leander_Name;
         Equations : Binding_Record_Lists.List;
      end record;

   package Name_Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Name_Binding);

   type Type_Binding is
      record
         Name      : Leander.Names.Leander_Name;
         Type_Expr : Leander.Syntax.Qualified_Types.Reference;
      end record;

   package Type_Binding_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Type_Binding);

   package Predicate_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Core.Predicates.Instance,
        Leander.Core.Predicates."=");

   type Instance is new Parent with
      record
         Context     : Core.Declaration_Context;
         Predicates  : Predicate_Lists.List;
         Bindings    : Name_Binding_Lists.List;
         Types       : Type_Binding_Lists.List;
         Monomorphic : Boolean := False;
      end record;

end Leander.Syntax.Bindings;
