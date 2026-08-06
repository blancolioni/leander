with Leander.Byte_Buffers;
with Leander.Core.Alts;
with Leander.Core.Binding_Groups;
with Leander.Core.Bindings;
with Leander.Core.Predicates;
with Leander.Core.Schemes.Serialize;
with Leander.Core.Types.Serialize;

package body Leander.Core.Type_Classes.Serialize is

   package BB renames Leander.Byte_Buffers;
   package TS renames Leander.Core.Types.Serialize;
   package SS renames Leander.Core.Schemes.Serialize;

   ------------
   -- Encode --
   ------------

   function Encode (This : Reference) return Ada.Streams.Stream_Element_Array is
      Node    : Instance renames Instance (This.all);
      Ps      : Leander.Core.Predicates.Predicate_Array renames Node.Predicates;
      Methods : constant Varid_Array := This.Methods;
      W       : BB.Writer;
   begin
      W.Put_String (Core.To_String (Node.Class_Id));
      W.Put_String (Core.To_String (Node.Var_Id));

      W.Put_U32 (Ps'Length);
      for P of Ps loop
         W.Put_String (P.Class_Name);
         TS.Put (W, P.Get_Type);
      end loop;

      W.Put_U32 (Methods'Length);
      for M of Methods loop
         W.Put_String (Core.To_String (M));
         W.Put_U8 (Boolean'Pos (This.Has_Default (M)));
         SS.Put (W, This.Method_Scheme (M));
      end loop;

      return W.To_Bytes;
   end Encode;

   ------------
   -- Decode --
   ------------

   function Decode (Bytes : Ada.Streams.Stream_Element_Array) return Reference is
      C        : BB.Offset := Bytes'First;
      Class_Id : constant Conid := Core.To_Conid (BB.Get_String (Bytes, C));
      Var_Id   : constant Varid := Core.To_Varid (BB.Get_String (Bytes, C));
      Pn       : constant Natural := BB.Get_U32 (Bytes, C);
      Ps       : Leander.Core.Predicates.Predicate_Array (1 .. Pn);
   begin
      for I in Ps'Range loop
         declare
            Name : constant String := BB.Get_String (Bytes, C);
            T    : constant Core.Types.Reference := TS.Get (Bytes, C);
         begin
            Ps (I) := Leander.Core.Predicates.Predicate (Name, T);
         end;
      end loop;

      declare
         Mn           : constant Natural := BB.Get_U32 (Bytes, C);
         Methods      : Leander.Core.Bindings.Reference_Array (1 .. Mn);
         Names        : Varid_Array (1 .. Mn);
         Has_Default  : array (1 .. Mn) of Boolean;
         Default_Count : Natural := 0;
         Builder      : Leander.Core.Binding_Groups.Instance_Builder;
      begin
         for I in Methods'Range loop
            declare
               Name   : constant Varid := Core.To_Varid (BB.Get_String (Bytes, C));
               Is_Default : constant Boolean := BB.Get_U8 (Bytes, C) /= 0;
               Scheme : constant Core.Schemes.Reference := SS.Get (Bytes, C);
            begin
               Names (I) := Name;
               Has_Default (I) := Is_Default;
               if Is_Default then
                  Default_Count := Default_Count + 1;
               end if;
               Methods (I) :=
                 Leander.Core.Bindings.Explicit_Binding
                   (Name, Leander.Core.Alts.Reference_Array'(1 .. 0 => <>), Scheme);
            end;
         end loop;

         if Methods'Length > 0 then
            Builder.Add_Explicit_Bindings (Methods);
         end if;

         declare
            Defaulted : Varid_Array (1 .. Default_Count);
            J         : Positive := 1;
         begin
            for I in Methods'Range loop
               if Has_Default (I) then
                  Defaulted (J) := Names (I);
                  J := J + 1;
               end if;
            end loop;

            return Type_Class
              (Class_Id, Var_Id, Ps, Builder.Get_Binding_Group, Defaulted);
         end;
      end;
   end Decode;

end Leander.Core.Type_Classes.Serialize;
