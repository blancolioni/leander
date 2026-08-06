with Leander.Core.Tycons;
with Leander.Core.Tyvars;

package body Leander.Core.Types.Serialize is

   package BB renames Leander.Byte_Buffers;

   Tag_TVar : constant := 0;
   Tag_TCon : constant := 1;
   Tag_TGen : constant := 2;
   Tag_TApp : constant := 3;

   Kind_Star : constant := 0;
   Kind_Fun  : constant := 1;

   ---------
   -- Put --
   ---------

   procedure Put
     (W : in out Leander.Byte_Buffers.Writer;
      T : Types.Reference)
   is
      Node : Instance renames Instance (T.all);
   begin
      case Node.Tag is
         when TVar =>
            W.Put_U8 (Tag_TVar);
            W.Put_String (Core.To_String (Tyvars.Name (Node.Tyvar)));
            Put_Kind (W, Tyvars.Kind (Node.Tyvar));
         when TCon =>
            W.Put_U8 (Tag_TCon);
            W.Put_String (Core.To_String (Tycons.Id (Node.Tycon)));
            Put_Kind (W, Node.Tycon.Get_Kind);
         when TGen =>
            W.Put_U8 (Tag_TGen);
            W.Put_U32 (Node.Index);
         when TApp =>
            W.Put_U8 (Tag_TApp);
            Put (W, Node.Left);
            Put (W, Node.Right);
      end case;
   end Put;

   ---------
   -- Get --
   ---------

   function Get
     (D : Leander.Byte_Buffers.Byte_Array;
      C : in out Leander.Byte_Buffers.Offset)
      return Types.Reference
   is
      Tag : constant Natural := BB.Get_U8 (D, C);
   begin
      case Tag is
         when Tag_TVar =>
            declare
               Name : constant String := BB.Get_String (D, C);
               K    : constant Kinds.Kind := Get_Kind (D, C);
            begin
               return Types.TVar (Tyvars.Tyvar (Core.To_Varid (Name), K));
            end;
         when Tag_TCon =>
            declare
               Name : constant String := BB.Get_String (D, C);
               K    : constant Kinds.Kind := Get_Kind (D, C);
            begin
               --  Well-known built-in constructors are cached singletons
               --  (see leander-core-types.adb), and Show's pretty-printer
               --  for arrow/list applications recognizes them by object
               --  identity (Reference "="), not by name -- so route these
               --  back through their own constructor rather than allocating
               --  a fresh, merely-structurally-equal Tycon.
               if Name = "()" then
                  return Types.T_Unit;
               elsif Name = "#error" then
                  return Types.T_Error;
               elsif Name = "Char" then
                  return Types.T_Char;
               elsif Name = "Int" then
                  return Types.T_Int;
               elsif Name = "Integer" then
                  return Types.T_Integer;
               elsif Name = "Float" then
                  return Types.T_Float;
               elsif Name = "Double" then
                  return Types.T_Double;
               elsif Name = "[]" then
                  return Types.T_List;
               elsif Name = "(->)" then
                  return Types.T_Arrow;
               elsif Name = "(,)" then
                  return Types.T_Pair;
               else
                  return Types.TCon (Tycons.Tycon (Core.To_Conid (Name), K));
               end if;
            end;
         when Tag_TGen =>
            return Types.TGen (BB.Get_U32 (D, C));
         when Tag_TApp =>
            declare
               L : constant Types.Reference := Get (D, C);
               R : constant Types.Reference := Get (D, C);
            begin
               return Types.Application (L, R);
            end;
         when others =>
            raise Program_Error with "bad type tag" & Tag'Image;
      end case;
   end Get;

   --------------
   -- Put_Kind --
   --------------

   procedure Put_Kind
     (W : in out Leander.Byte_Buffers.Writer;
      K : Leander.Core.Kinds.Kind)
   is
   begin
      if Kinds.Is_Star (K) then
         W.Put_U8 (Kind_Star);
      else
         W.Put_U8 (Kind_Fun);
         Put_Kind (W, Kinds.Left_Kind (K));
         Put_Kind (W, Kinds.Right_Kind (K));
      end if;
   end Put_Kind;

   --------------
   -- Get_Kind --
   --------------

   function Get_Kind
     (D : Leander.Byte_Buffers.Byte_Array;
      C : in out Leander.Byte_Buffers.Offset)
      return Leander.Core.Kinds.Kind
   is
      Tag : constant Natural := BB.Get_U8 (D, C);
   begin
      if Tag = Kind_Star then
         return Kinds.Star;
      else
         declare
            L : constant Kinds.Kind := Get_Kind (D, C);
            R : constant Kinds.Kind := Get_Kind (D, C);
         begin
            return Kinds.Kind_Function (L, R);
         end;
      end if;
   end Get_Kind;

end Leander.Core.Types.Serialize;
