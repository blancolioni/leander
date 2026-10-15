with Leander.Byte_Buffers;
with Leander.Core.Schemes.Serialize;
with Leander.Core.Types.Serialize;
with Leander.Data_Types.Builder;

package body Leander.Data_Types.Serialize is

   package BB renames Leander.Byte_Buffers;
   package TS renames Leander.Core.Types.Serialize;
   package SS renames Leander.Core.Schemes.Serialize;

   type Nullable_Scheme is access constant Core.Schemes.Instance'Class;

   ------------
   -- Encode --
   ------------

   function Encode (This : Reference) return Ada.Streams.Stream_Element_Array is
      W : BB.Writer;
   begin
      TS.Put (W, This.Applied_Type);
      W.Put_U8 (Boolean'Pos (This.Is_Newtype));
      W.Put_U32 (This.Constructor_Count);
      for I in 1 .. This.Constructor_Count loop
         W.Put_String (Core.To_String (This.Constructor_Name (I)));
         SS.Put (W, This.Constructor_Type (I));
      end loop;

      --  Field labels (#33) come after everything else, so that an image
      --  written before them still decodes, with no labels.
      for I in 1 .. This.Constructor_Count loop
         declare
            Labels : constant Leander.Names.Name_Array :=
                       This.Constructor_Labels (I);
         begin
            W.Put_U32 (Labels'Length);
            for Label of Labels loop
               W.Put_String (Leander.Names.To_String (Label));
            end loop;
         end;
      end loop;

      return W.To_Bytes;
   end Encode;

   ------------
   -- Decode --
   ------------

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Reference is
      use type Ada.Streams.Stream_Element_Offset;
      C       : BB.Offset := Bytes'First;
      Applied : constant Core.Types.Reference := TS.Get (Bytes, C);
      Newtype : constant Boolean := BB.Get_U8 (Bytes, C) /= 0;
      Cn      : constant Natural := BB.Get_U32 (Bytes, C);
      Builder : Leander.Data_Types.Builder.Data_Type_Builder;
      Names   : array (1 .. Cn) of Core.Conid;
      Schemes : array (1 .. Cn) of Nullable_Scheme;
   begin
      Builder.Start (Applied, Is_Newtype => Newtype);
      for I in 1 .. Cn loop
         Names (I) := Core.To_Conid (BB.Get_String (Bytes, C));
         Schemes (I) := Nullable_Scheme (SS.Get (Bytes, C));
      end loop;

      for I in 1 .. Cn loop
         if C > Bytes'Last then
            Builder.Add_Con (Names (I), Core.Schemes.Reference (Schemes (I)));
         else
            declare
               Count  : constant Natural := BB.Get_U32 (Bytes, C);
               Labels : Leander.Names.Name_Array (1 .. Count);
            begin
               for J in Labels'Range loop
                  Labels (J) :=
                    Leander.Names.To_Leander_Name (BB.Get_String (Bytes, C));
               end loop;
               Builder.Add_Con
                 (Names (I), Core.Schemes.Reference (Schemes (I)), Labels);
            end;
         end if;
      end loop;
      Builder.Build;
      return Builder.Data_Type;
   end Decode;

end Leander.Data_Types.Serialize;
