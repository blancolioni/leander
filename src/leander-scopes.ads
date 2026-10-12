with Ada.Strings.Unbounded;

private with Ada.Containers.Indefinite_Hashed_Maps;
private with Ada.Containers.Indefinite_Vectors;
private with Ada.Strings.Hash;

with Leander.Environment;
with Leander.Source;

package Leander.Scopes is

   --  A module's scope table: what the programmer of that module may
   --  write, and what each written name means. It is the parser's
   --  authority on naming, deliberately separate from what the linker can
   --  reach -- Environment.Import copies the constructor, type and class
   --  maps wholesale, because a name list governs what may be written, not
   --  what a compiled binding may resolve against.

   type Instance is tagged limited private;
   type Reference is access all Instance;

   function New_Scope return Reference;

   type Name_Space is
     (Value_Space, Constructor_Space, Type_Space, Class_Space);

   procedure Add_Module
     (This  : in out Instance;
      Alias : String;
      Env   : not null Leander.Environment.Reference);
   --  Make Env's names reachable as "Alias.name". Alias is the "as" name
   --  when an import declaration gave one, otherwise the module's own
   --  name -- which is Haskell's rule: "import M as S" makes S.foo legal
   --  and M.foo not.

   function Is_Qualified (Written : String) return Boolean;
   --  True when Written carries a module qualifier, i.e. it has a dot and
   --  what precedes that dot looks like a module name. Note this is about
   --  the written form alone; whether the qualifier names anything is
   --  Resolve's business.

   procedure Resolve
     (This    : Instance;
      Written : String;
      Space   : Name_Space;
      Name    : out Ada.Strings.Unbounded.Unbounded_String;
      Message : out Ada.Strings.Unbounded.Unbounded_String);
   --  Name is what the rest of the compiler should use for Written.
   --  Message is empty on success, and otherwise says why Written could
   --  not be resolved -- in which case Name still falls back to Written,
   --  so that a parse can carry on and report more than one problem.
   --
   --  An unqualified name is returned unchanged: until every top-level
   --  name carries its module, the scope table cannot tell a name this
   --  module declares from one a lambda binds, so it does not try.

   procedure Defer_Type
     (This     : in out Instance;
      Name     : String;
      Location : Leander.Source.Source_Location);
   --  Name was written as a type constructor where no type of that name
   --  was in scope yet. It may still be declared further down the module,
   --  so it is checked once the module's declarations are all in, rather
   --  than here (#142).

   procedure Iterate_Deferred_Types
     (This    : Instance;
      Process : not null access
        procedure (Name     : String;
                   Location : Leander.Source.Source_Location));

private

   use Ada.Strings.Unbounded;

   package Module_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Leander.Environment.Reference,
        Hash            => Ada.Strings.Hash,
        Equivalent_Keys => "=",
        "="             => Leander.Environment."=");

   type Deferred_Type (Length : Natural) is
      record
         Location : Leander.Source.Source_Location;
         Name     : String (1 .. Length);
      end record;

   package Deferred_Type_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Deferred_Type);

   type Instance is tagged limited
      record
         Modules        : Module_Maps.Map;
         Deferred_Types : Deferred_Type_Vectors.Vector;
      end record;

end Leander.Scopes;
