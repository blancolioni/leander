with Leander.Byte_Buffers;
with Leander.Core.Schemes.Serialize;
with Leander.Core.Types.Serialize;
with Leander.Data_Types.Builder;

package body Leander.Data_Types.Serialize is

   package BB renames Leander.Byte_Buffers;
   package TS renames Leander.Core.Types.Serialize;
   package SS renames Leander.Core.Schemes.Serialize;

   ------------
   -- Encode --
   ------------

   function Encode (This : Reference) return Ada.Streams.Stream_Element_Array is
      W : BB.Writer;
   begin
      TS.Put (W, This.Applied_Type);
      W.Put_U32 (This.Constructor_Count);
      for I in 1 .. This.Constructor_Count loop
         W.Put_String (Core.To_String (This.Constructor_Name (I)));
         SS.Put (W, This.Constructor_Type (I));
      end loop;

      return W.To_Bytes;
   end Encode;

   ------------
   -- Decode --
   ------------

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Reference is
      C       : BB.Offset := Bytes'First;
      Applied : constant Core.Types.Reference := TS.Get (Bytes, C);
      Cn      : constant Natural := BB.Get_U32 (Bytes, C);
      Builder : Leander.Data_Types.Builder.Data_Type_Builder;
   begin
      Builder.Start (Applied);
      for I in 1 .. Cn loop
         declare
            Name   : constant Core.Conid := Core.To_Conid (BB.Get_String (Bytes, C));
            Scheme : constant Core.Schemes.Reference := SS.Get (Bytes, C);
         begin
            Builder.Add_Con (Name, Scheme);
         end;
      end loop;
      Builder.Build;
      return Builder.Data_Type;
   end Decode;

end Leander.Data_Types.Serialize;
