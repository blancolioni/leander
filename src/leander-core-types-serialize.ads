with Leander.Byte_Buffers;
with Leander.Core.Kinds;

package Leander.Core.Types.Serialize is

   --  Binary (de)serialization of a type tree (TVar / TCon / TGen / TApp)
   --  and of Kind, for Leander.Core.Schemes.Serialize.  Operates on a shared
   --  Leander.Byte_Buffers cursor rather than a self-contained byte array,
   --  so several types (a predicate's argument, a scheme's body) can be
   --  packed into one buffer without individual length framing.

   procedure Put
     (W : in out Leander.Byte_Buffers.Writer;
      T : Types.Reference);

   function Get
     (D : Leander.Byte_Buffers.Byte_Array;
      C : in out Leander.Byte_Buffers.Offset)
      return Types.Reference;

   procedure Put_Kind
     (W : in out Leander.Byte_Buffers.Writer;
      K : Leander.Core.Kinds.Kind);

   function Get_Kind
     (D : Leander.Byte_Buffers.Byte_Array;
      C : in out Leander.Byte_Buffers.Offset)
      return Leander.Core.Kinds.Kind;

end Leander.Core.Types.Serialize;
