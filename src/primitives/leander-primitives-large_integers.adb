with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;

package body Leander.Primitives.Large_Integers is

   package Large_Integer_Lists is
      new Ada.Containers.Doubly_Linked_Lists (Large_Integer_Object);

   ---------
   -- Add --
   ---------

   procedure Add
     (X : in out Large_Integer_Object'Class;
      Y : Large_Integer_Object'Class)
   is
      Carry : Element_Type := 0;
      Max   : constant Natural :=
                Natural'Max (X.Xs.Last_Index, Y.Xs.Last_Index);
      Neg   : constant Boolean := Y.Negative;
      Result : Large_Integer_Object'Class := Zero;
   begin
      if X.Negative and then Y.Negative then
         declare
            X1 : Large_Integer_Object'Class := X;
            Y1 : Large_Integer_Object'Class := Y;
         begin
            X1.Negative := False;
            Y1.Negative := False;
            X1.Add (Y1);
            X1.Negative := True;
            return;
         end;
      elsif X.Negative then
         declare
            X1 : Large_Integer_Object'Class := X;
            Y1 : Large_Integer_Object'Class := Y;
         begin
            X1.Negative := False;
            Y1.Negative := True;
            X1.Add (Y1);
            X1.Negative := not X1.Negative;
            X := X1;
            return;
         end;
      elsif Y.Negative then
         declare
            X1 : Large_Integer_Object'Class := X;
            Y1 : Large_Integer_Object'Class := Y;
         begin
            Y1.Negative := False;
            if X1.Compare (Y1) = LT then
               X1.Negative := True;
               Y1.Add (X1);
               Y1.Negative := True;
               X := Y1;
               return;
            end if;
         end;
      end if;

      for I in 1 .. Max loop
         declare
            A : constant Element_Type :=
                  (if I <= X.Xs.Last_Index then X.Xs (I) else 0);
            B : constant Element_Type :=
                  (if I <= Y.Xs.Last_Index then Y.Xs (I) else 0);
            C : constant Element_Type := Carry;
            R : Element_Type;
         begin

            if Neg then
               Carry := 0;
               R := A - B;
               if A < B then
                  Carry := 1;
               end if;
               if R < C then
                  Carry := Carry + 1;
               end if;
               R := R - C;
               Result.Xs.Append (R);
            else
               Carry := 0;
               R := A + B;
               if Element_Type'Last - A < B then
                  Carry := 1;
               end if;
               if Element_Type'Last - R < C then
                  Carry := Carry + 1;
               end if;
               R := R + C;
               Result.Xs.Append (R);
            end if;
         end;
      end loop;
      if Carry > 0 then
         Result.Xs.Append (Carry);
         if Neg then
            Result.Negative := True;
         end if;
      end if;
      X := Result;
   end Add;

   ---------
   -- Add --
   ---------

   procedure Add
     (X : in out Large_Integer_Object'Class;
      Y : Integer)
   is
      Y1 : Large_Integer_Object;
   begin
      Y1.Negative := Y < 0;
      Y1.Xs.Append (Element_Type (abs Y));
      X.Add (Y1);
   end Add;

   -------------
   -- Compare --
   -------------

   function Compare
     (X, Y : Large_Integer_Object'Class)
      return Compare_Result
   is
   begin
      if X.Negative /= Y.Negative then
         if X.Negative then
            return LT;
         else
            return GT;
         end if;
      end if;

      for I in reverse 1 .. Natural'Max (X.Xs.Last_Index, Y.Xs.Last_Index) loop
         declare
            X_Elem : constant Element_Type :=
                       (if I <= X.Xs.Last_Index then X.Xs (I) else 0);
            Y_Elem : constant Element_Type :=
                       (if I <= Y.Xs.Last_Index then Y.Xs (I) else 0);
         begin
            if X_Elem < Y_Elem then
               return (if X.Negative then GT else LT);
            elsif X_Elem > Y_Elem then
               return (if X.Negative then LT else GT);
            end if;
         end;
      end loop;

      return EQ;

   end Compare;

   ------------
   -- Divide --
   ------------

   procedure Divide
     (X         : in out Large_Integer_Object'Class;
      Y         : Integer;
      Remainder : out Integer)
   is
      Negative : Boolean := False;
      Divisor  : Integer := Y;
   begin

      if Divisor = 0 then
         raise Evaluation_Error with "division by zero";
      elsif Divisor = 1 then
         Remainder := 0;
         return;
      elsif Divisor = -1 then
         X.Negative := not X.Negative;
         Remainder := 0;
         return;
      elsif Divisor < 0 then
         Negative := True;
         Divisor := -Divisor;
      end if;

      declare
         type Partial_Dividend is mod 2 ** (Element_Bits * 2);
         R     : Element_Type := 0;
         Non_Zero : Boolean := False;
      begin

         for I in reverse 1 .. X.Xs.Last_Index loop
            declare
               D : constant Partial_Dividend :=
                     Partial_Dividend (R)
                     * 2 ** Element_Bits
                     + Partial_Dividend (X.Xs.Element (I));
               Q : constant Partial_Dividend :=
                     D / Partial_Dividend (Divisor);
            begin
               if Q = 0 and then not Non_Zero then
                  X.Xs.Delete_Last;
               else
                  X.Xs (I) := Element_Type (Q);
                  Non_Zero := True;
               end if;
               R := Element_Type (D mod Partial_Dividend (Divisor));
               Remainder := Integer (R);
            end;
         end loop;
      end;

      if Negative then
         X.Negative := not X.Negative;
      end if;

   end Divide;

   -----------
   -- Equal --
   -----------

   overriding function Equal
     (X, Y  : Large_Integer_Object;
      Store : SK.Objects.Object_Store'Class)
      return Boolean
   is
      pragma Unreferenced (Store);
   begin
      return X = Y;
   end Equal;

   ----------------------
   -- In_Integer_Range --
   ----------------------

   function In_Integer_Range
     (X : Large_Integer_Object'Class)
      return Boolean
   is
   begin
      return X.Xs.Last_Index = 0 or else
        (X.Xs.Last_Index = 1
           and then X.Xs (1) <= SK.Max_Integer);
   end In_Integer_Range;

   ----------------------
   -- Is_Large_Integer --
   ----------------------

   function Is_Large_Integer
     (Store : SK.Objects.Object_Store'Class;
      Item  : SK.Objects.Object)
      return Boolean
   is
   begin
      return SK.Objects.Is_External_Object (Item)
        and then Store.Get_External_Object (Item).all
      in Large_Integer_Object'Class;
   end Is_Large_Integer;

   -------------
   -- Is_Zero --
   -------------

   function Is_Zero
     (X : Large_Integer_Object'Class)
      return Boolean
   is
   begin
      return X.Xs.Last_Index = 0;
   end Is_Zero;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (X : in out Large_Integer_Object'Class;
      Y : Integer)
   is
      Negative : Boolean := False;
      Multiplier : Integer := Y;
   begin

      if Multiplier = 0 then
         X.Xs.Clear;
         X.Negative := False;
         return;
      elsif Multiplier = 1 then
         return;
      elsif Multiplier = -1 then
         X.Negative := not X.Negative;
         return;
      elsif Multiplier < 0 then
         Negative := True;
         Multiplier := -Multiplier;
      end if;

      declare
         Extra_Adds : Large_Integer_Lists.List;
      begin
         while Multiplier > 1 loop
            if Multiplier mod 2 = 0 then
               X.Add (X);
               Multiplier := Multiplier / 2;
            else
               Multiplier := Multiplier - 1;
               Extra_Adds.Append (Large_Integer_Object (X));
            end if;
         end loop;

         for Z of Extra_Adds loop
            X.Add (Z);
         end loop;
      end;

      if Negative then
         X.Negative := not X.Negative;
      end if;

   end Multiply;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Item : Large_Integer_Object)
      return String
   is
      pragma Unreferenced (Item);
   begin
      return "large-integer";
   end Name;

   ------------
   -- Negate --
   ------------

   procedure Negate (X : in out Large_Integer_Object'Class) is
   begin
      X.Negative := not X.Negative;
   end Negate;

   -----------
   -- Print --
   -----------

   overriding function Print
     (Item  : Large_Integer_Object;
      Store : in out SK.Objects.Object_Store'Class)
      return String
   is
      pragma Unreferenced (Store);
      use Ada.Strings.Unbounded;
      Acc : Large_Integer_Object := Item;
      Text : Unbounded_String;
      R    : Integer;
   begin
      if Acc.Is_Zero then
         return "0";
      else
         while not Acc.Is_Zero loop
            Acc.Divide (10, R);
            declare
               Ch : constant Character :=
                      Character'Val (R + 48);
            begin
               Text := Ch & Text;
            end;
         end loop;
         if Item.Negative then
            Text := '-' & Text;
         end if;
         return To_String (Text);
      end if;
   end Print;

   ----------------
   -- To_Integer --
   ----------------

   function To_Integer (X : Large_Integer_Object'Class) return Integer is
   begin
      if X.Xs.Is_Empty then
         return 0;
      elsif X.Negative then
         return -Integer (X.Xs.First_Element);
      else
         return Integer (X.Xs.First_Element);
      end if;
   end To_Integer;

   ----------------------
   -- To_Large_Integer --
   ----------------------

   function To_Large_Integer
     (Item : Integer)
      return Large_Integer_Object'Class
   is
   begin
      return Result : Large_Integer_Object do
         Result.Negative := Item < 0;
         Result.Xs.Append (Element_Type (abs Item));
      end return;
   end To_Large_Integer;

   ----------------------
   -- To_Large_Integer --
   ----------------------

   function To_Large_Integer
     (Store : in out SK.Objects.Object_Store'Class;
      Item  : SK.Objects.Object)
      return Large_Integer_Object'Class
   is
   begin
      if SK.Objects.Is_External_Object (Item) then
         return Large_Integer_Object'Class
           (Store.Get_External_Object (Item).all);
      elsif SK.Objects.Is_Integer (Item) then
         return To_Large_Integer (SK.Objects.To_Integer (Item));
      else
         raise Evaluation_Error with
           "can't convert to large integer: "
           & Store.Show (Item);
      end if;
   end To_Large_Integer;

   ---------------
   -- To_Object --
   ---------------

   function To_Object
     (Store : in out SK.Objects.Object_Store'Class;
      Value : Large_Integer_Object'Class)
      return SK.Objects.Object
   is
   begin
      if Value.In_Integer_Range then
         return SK.Objects.To_Object (Integer'(Value.To_Integer));
      else
         return Store.Create_External_Reference
           (Value);
      end if;
   end To_Object;

end Leander.Primitives.Large_Integers;
