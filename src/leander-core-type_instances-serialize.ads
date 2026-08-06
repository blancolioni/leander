with Ada.Streams;

package Leander.Core.Type_Instances.Serialize is

   --  Binary (de)serialization of an instance *fact* -- the class id, its
   --  constraint predicates (the "Eq a =>" context), and the instance head
   --  type ("Eq [a]") -- for the .skix Annotations section. This is
   --  everything Get_Instances/Entails/Reduce need to resolve context
   --  reduction against a decoded instance at future inference time.
   --
   --  The instance's own method bindings are not part of this type (see
   --  Leander.Environment's private Instance_Record) and are not encoded
   --  here: they're only ever consumed by Elaborate_Instance, which a
   --  module loaded whole from a complete .skix image never runs -- its
   --  dictionary values are already compiled and reachable as ordinary
   --  graph content in the image.

   function Encode (This : Reference) return Ada.Streams.Stream_Element_Array;

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Reference;

end Leander.Core.Type_Instances.Serialize;
