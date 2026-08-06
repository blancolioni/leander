with Leander.Byte_Buffers;
with Leander.Core.Predicates;
with Leander.Core.Types.Serialize;

package body Leander.Core.Type_Instances.Serialize is

   package BB renames Leander.Byte_Buffers;
   package TS renames Leander.Core.Types.Serialize;

   ------------
   -- Encode --
   ------------

   function Encode (This : Reference) return Ada.Streams.Stream_Element_Array is
      Cs : constant Leander.Core.Predicates.Predicate_Array :=
             This.Qualifier.Predicates;
      W  : BB.Writer;
   begin
      W.Put_String (This.Predicate.Class_Name);
      TS.Put (W, This.Predicate.Get_Type);

      W.Put_U32 (Cs'Length);
      for C of Cs loop
         W.Put_String (C.Class_Name);
         TS.Put (W, C.Get_Type);
      end loop;

      return W.To_Bytes;
   end Encode;

   ------------
   -- Decode --
   ------------

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Reference is
      C          : BB.Offset := Bytes'First;
      Class_Name : constant String := BB.Get_String (Bytes, C);
      Inst_Type  : constant Core.Types.Reference := TS.Get (Bytes, C);
      Cn         : constant Natural := BB.Get_U32 (Bytes, C);
      Cs         : Leander.Core.Predicates.Predicate_Array (1 .. Cn);
   begin
      for I in Cs'Range loop
         declare
            Name : constant String := BB.Get_String (Bytes, C);
            T    : constant Core.Types.Reference := TS.Get (Bytes, C);
         begin
            Cs (I) := Leander.Core.Predicates.Predicate (Name, T);
         end;
      end loop;

      return Make_Instance
        (Cs, Leander.Core.Predicates.Predicate (Class_Name, Inst_Type));
   end Decode;

end Leander.Core.Type_Instances.Serialize;
