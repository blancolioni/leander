with Ada.Streams;

package Leander.Data_Types.Serialize is

   --  Binary (de)serialization of a data type declaration (the applied
   --  type head, e.g. "Maybe a", plus each constructor's name and Scheme)
   --  for the .skix Annotations section. A constructor's synthesized
   --  curried-lambda body (Constructor_Calculus) is not encoded: it is
   --  always mechanically regenerated from the constructor's arity, so
   --  Decode drives Leander.Data_Types.Builder exactly as
   --  Leander.Environment.Prelude.Create already does by hand for the
   --  built-in ()/(,)/Bool/[] types.

   function Encode (This : Reference) return Ada.Streams.Stream_Element_Array;

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Reference;

end Leander.Data_Types.Serialize;
