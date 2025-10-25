with Ada.Containers.Vectors;

package body Leander.Core.Kinds is

   type KFun is
      record
         Left, Right : Kind;
      end record;

   subtype KFun_Index is Kind range 1 .. Kind'Last;

   package KFun_Vectors is
     new Ada.Containers.Vectors (KFun_Index, KFun);

   KFun_Vector : KFun_Vectors.Vector;

   -------------------
   -- Kind_Function --
   -------------------

   function Kind_Function (Left, Right : Kind) return Kind is
      Rec : constant KFun := (Left, Right);
   begin
      for I in 1 .. KFun_Vector.Last_Index loop
         if KFun_Vector (I) = Rec then
            return I;
         end if;
      end loop;
      KFun_Vector.Append (KFun'(Left, Right));
      return KFun_Vector.Last_Index;
   end Kind_Function;

   ---------------
   -- Left_Kind --
   ---------------

   function Left_Kind (K : Kind) return Kind is
   begin
      return KFun_Vector (K).Left;
   end Left_Kind;

   ----------------
   -- Right_Kind --
   ----------------

   function Right_Kind (K : Kind) return Kind is
   begin
      return KFun_Vector (K).Right;
   end Right_Kind;

   ----------
   -- Show --
   ----------

   function Show (K : Kind) return String is
   begin
      if K = Star then
         return "*";
      else
         declare
            This : KFun renames KFun_Vector (K);
         begin
            if This.Right = Star then
               return Show (This.Left) & "->*";
            else
               return Show (This.Left) & "->(" & Show (This.Right) & ")";
            end if;
         end;
      end if;
   end Show;

end Leander.Core.Kinds;
