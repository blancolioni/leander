private with Ada.Containers.Vectors;
with Ada.Streams;

package Leander.Byte_Buffers is

   use type Ada.Streams.Stream_Element;
   use type Ada.Streams.Stream_Element_Offset;

   --  A small growable byte buffer for writing a self-contained binary
   --  encoding, and matching cursor-based readers over the resulting bytes.
   --  Used to encode Leander's inferred types as the opaque per-export
   --  annotation bytes of a Skit module image (skit/docs/module-image-format.md);
   --  Skit never interprets this encoding, so its exact shape is Leander's own
   --  choice.

   subtype Byte is Ada.Streams.Stream_Element;
   subtype Offset is Ada.Streams.Stream_Element_Offset;
   subtype Byte_Array is Ada.Streams.Stream_Element_Array;

   type Writer is tagged private;

   procedure Put_U8
     (This : in out Writer;
      X    : Natural)
     with Pre => X <= 255;

   procedure Put_U32
     (This : in out Writer;
      X    : Natural);

   procedure Put_String
     (This : in out Writer;
      S    : String);
   --  A u32 length prefix followed by the string's bytes (Latin-1: one byte
   --  per Character, matching Leander's identifier character set).

   function To_Bytes (This : Writer) return Byte_Array;

   function Get_U8
     (D : Byte_Array;
      C : in out Offset)
      return Natural;

   function Get_U32
     (D : Byte_Array;
      C : in out Offset)
      return Natural;

   function Get_String
     (D : Byte_Array;
      C : in out Offset)
      return String;

private

   package Byte_Vectors is new Ada.Containers.Vectors (Natural, Byte);

   type Writer is tagged
      record
         Data : Byte_Vectors.Vector;
      end record;

end Leander.Byte_Buffers;
