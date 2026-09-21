with Leander.Core.Types;

package Leander.Core.Type_Synonyms is

   --  A type synonym is a compile-time rewrite and nothing else: it has no
   --  runtime representation, no constructors, and never reaches inference.
   --
   --  The table is global to the process rather than per environment, for
   --  the same reason the fixity table is: expansion happens inside
   --  Syntax.Types.To_Core, which has no environment to consult.  It is
   --  never cleared between handles -- a second Leander.Create reuses an
   --  already-registered Prelude rather than reparsing it, so clearing
   --  would throw away synonyms nothing would put back.  Two modules can
   --  therefore see each other's synonyms regardless of what their import
   --  and export lists say -- unlike data types and values, which #66
   --  gave per-module keys.  Scoping synonyms (and fixities, which have
   --  the same shape) is separate work; see
   --  share/leander/docs/modules.md.
   --
   --  A declaration stores its right-hand side with each parameter replaced
   --  by a TGen index, so expanding an application is Types.Instantiate over
   --  the argument types -- the same mechanism a Scheme already uses.
   --
   --  Definitions are stored already expanded, since the type expression a
   --  declaration is built from has itself been through Expand.  A synonym
   --  can therefore be written in terms of an earlier one without Expand
   --  having to iterate to a fixed point.

   procedure Add
     (Name       : Conid;
      Parameters : Varid_Array;
      Definition : Leander.Core.Types.Reference);

   function Exists (Name : Conid) return Boolean;

   function Arity (Name : Conid) return Natural
     with Pre => Exists (Name);

   function Expand
     (T : Leander.Core.Types.Reference)
      return Leander.Core.Types.Reference;
   --  Rewrite T's own application spine if its head is a fully applied
   --  synonym, and return T unchanged otherwise.  Only the spine at the top
   --  of T is considered: the callers are the To_Core traversals, which
   --  build a type bottom up and so have already expanded every child.
   --
   --  A synonym applied to fewer arguments than its arity is left alone, so
   --  it surfaces later as an unknown type constructor.  Haskell rejects a
   --  partially applied synonym too.

   --  Iteration, for writing the declarations into a .skix image.  A module
   --  loaded from an image has never been parsed, so its synonyms have to
   --  travel with it or a downstream module could not name them.

   function Count return Natural;

   function Synonym_Name (Index : Positive) return Conid
     with Pre => Index <= Count;

   function Synonym_Arity (Index : Positive) return Natural
     with Pre => Index <= Count;

   function Synonym_Definition
     (Index : Positive)
      return Leander.Core.Types.Reference
     with Pre => Index <= Count;
   --  The definition as stored: parameters appear as TGen 1 .. Arity.

   procedure Add_Expanded
     (Name       : Conid;
      Arity      : Natural;
      Definition : Leander.Core.Types.Reference);
   --  Add a synonym whose definition is already in TGen form, for restoring
   --  one from an image rather than from source.

   procedure Clear;

end Leander.Core.Type_Synonyms;
