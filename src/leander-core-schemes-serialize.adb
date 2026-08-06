with Leander.Byte_Buffers;
with Leander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Core.Types.Serialize;

package body Leander.Core.Schemes.Serialize is

   package BB renames Leander.Byte_Buffers;
   package TS renames Leander.Core.Types.Serialize;

   ------------
   -- Encode --
   ------------

   function Encode (This : Schemes.Reference) return Ada.Streams.Stream_Element_Array is
      Node : Instance renames Instance (This.all);
      Ps   : constant Leander.Core.Predicates.Predicate_Array :=
               Node.QT.Predicates;
      W    : BB.Writer;
   begin
      W.Put_U32 (Node.Ks'Length);
      for K of Node.Ks loop
         TS.Put_Kind (W, K);
      end loop;

      W.Put_U32 (Ps'Length);
      for P of Ps loop
         W.Put_String (P.Class_Name);
         TS.Put (W, P.Get_Type);
      end loop;

      TS.Put (W, Node.QT.Get_Type);

      return W.To_Bytes;
   end Encode;

   ------------
   -- Decode --
   ------------

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Schemes.Reference is
      C  : BB.Offset := Bytes'First;
      Kn : constant Natural := BB.Get_U32 (Bytes, C);
      Ks : Kind_Array (1 .. Kn);
   begin
      for I in Ks'Range loop
         Ks (I) := TS.Get_Kind (Bytes, C);
      end loop;

      declare
         Pn : constant Natural := BB.Get_U32 (Bytes, C);
         Ps : Leander.Core.Predicates.Predicate_Array (1 .. Pn);
      begin
         for I in Ps'Range loop
            declare
               Name : constant String := BB.Get_String (Bytes, C);
               T    : constant Types.Reference := TS.Get (Bytes, C);
            begin
               Ps (I) := Leander.Core.Predicates.Predicate (Name, T);
            end;
         end loop;

         declare
            Body_T : constant Types.Reference := TS.Get (Bytes, C);
            QT     : constant Qualified_Types.Reference :=
                       Leander.Core.Qualified_Types.Qualified_Type
                         (Ps, Body_T);
         begin
            return From_Parts (Ks, QT);
         end;
      end;
   end Decode;

end Leander.Core.Schemes.Serialize;
