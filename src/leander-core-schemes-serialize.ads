with Ada.Streams;

package Leander.Core.Schemes.Serialize is

   --  Binary (de)serialization of a whole Scheme (quantified kinds,
   --  predicates, and body type -- see Leander.Core.Types.Serialize for the
   --  type-tree encoding).  This is Leander's own opaque payload for a
   --  Skit module image's per-export Annotations section
   --  (skit/docs/module-image-format.md): Skit never parses these bytes, so
   --  the encoding is free to be exact and non-human-readable rather than
   --  round-tripping through Scheme.Show and a type-signature parser (there
   --  is none).

   function Encode (This : Schemes.Reference) return Ada.Streams.Stream_Element_Array;

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Schemes.Reference;

end Leander.Core.Schemes.Serialize;
