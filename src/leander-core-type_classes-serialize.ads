with Ada.Streams;

package Leander.Core.Type_Classes.Serialize is

   --  Binary (de)serialization of a class declaration (class id, class
   --  variable, superclass predicates, and method names/schemes) for the
   --  .skix Annotations section -- see Leander.Core.Schemes.Serialize for
   --  the sibling per-export Scheme encoding this builds on.
   --
   --  Method bodies are not encoded: the class's Bindings only ever need to
   --  answer Method_Scheme, and a decoded class's method entries are
   --  reconstructed as zero-Alt Explicit_Bindings (a Scheme with no body) --
   --  the compiled "$inst" dispatch selectors that Environment.Type_Class
   --  builds from Methods are what downstream code actually calls, and
   --  those already travel as ordinary compiled Skit values.

   function Encode (This : Reference) return Ada.Streams.Stream_Element_Array;

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Reference;

end Leander.Core.Type_Classes.Serialize;
