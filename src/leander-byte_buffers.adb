with Interfaces;

package body Leander.Byte_Buffers is

   use type Interfaces.Unsigned_32;

   ------------
   -- Put_U8 --
   ------------

   procedure Put_U8
     (This : in out Writer;
      X    : Natural)
   is
   begin
      This.Data.Append (Byte (X));
   end Put_U8;

   -------------
   -- Put_U32 --
   -------------

   procedure Put_U32
     (This : in out Writer;
      X    : Natural)
   is
      V : constant Interfaces.Unsigned_32 := Interfaces.Unsigned_32 (X);
   begin
      for I in 0 .. 3 loop
         This.Data.Append
           (Byte (Interfaces.Shift_Right (V, I * 8) and 16#FF#));
      end loop;
   end Put_U32;

   ----------------
   -- Put_String --
   ----------------

   procedure Put_String
     (This : in out Writer;
      S    : String)
   is
   begin
      This.Put_U32 (S'Length);
      for Ch of S loop
         This.Data.Append (Byte (Character'Pos (Ch)));
      end loop;
   end Put_String;

   --------------
   -- To_Bytes --
   --------------

   function To_Bytes (This : Writer) return Byte_Array is
      Result : Byte_Array (1 .. Offset (This.Data.Length));
   begin
      for I in Result'Range loop
         Result (I) := This.Data (Natural (I - Result'First));
      end loop;
      return Result;
   end To_Bytes;

   -----------
   -- Get_U8 --
   -----------

   function Get_U8
     (D : Byte_Array;
      C : in out Offset)
      return Natural
   is
   begin
      return R : constant Natural := Natural (D (C)) do
         C := C + 1;
      end return;
   end Get_U8;

   ------------
   -- Get_U32 --
   ------------

   function Get_U32
     (D : Byte_Array;
      C : in out Offset)
      return Natural
   is
      V : Interfaces.Unsigned_32 := 0;
   begin
      for I in 0 .. 3 loop
         V := V or
           Interfaces.Shift_Left
             (Interfaces.Unsigned_32 (D (C + Offset (I))), I * 8);
      end loop;
      C := C + 4;
      return Natural (V);
   end Get_U32;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (D : Byte_Array;
      C : in out Offset)
      return String
   is
      Len    : constant Natural := Get_U32 (D, C);
      Result : String (1 .. Len);
   begin
      for I in Result'Range loop
         Result (I) := Character'Val (Natural (D (C)));
         C := C + 1;
      end loop;
      return Result;
   end Get_String;

end Leander.Byte_Buffers;
