with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

with Leander.Errors;

package body Leander.Core.Cases is

   type Algebraic_Alt is
      record
         Pat, Exp : Leander.Core.Trees.Tree_Type;
      end record;

   package List_Of_Con_Alts is
     new Ada.Containers.Doubly_Linked_Lists (Algebraic_Alt);

   package Algebraic_Con_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => List_Of_Con_Alts.List,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=",
        "="             => List_Of_Con_Alts."=");

   function Transform_Algebraic_Con
     (Alts : List_Of_Con_Alts.List)
      return Leander.Core.Trees.Tree_Type;

   -------------
   -- Add_Alt --
   -------------

   procedure Add_Alt
     (Builder    : in out Case_Builder'Class;
      Pattern    : Leander.Core.Trees.Tree_Type;
      Expression : Leander.Core.Trees.Tree_Type)
   is
   begin
      Builder.Pats.Append (Pattern);
      Builder.Exps.Append (Expression);
   end Add_Alt;

   -------------------------
   -- Set_Case_Expression --
   -------------------------

   procedure Set_Case_Expression
     (Builder    : in out Case_Builder'Class;
      Expression : Leander.Core.Trees.Tree_Type)
   is
   begin
      Builder.Case_Expr := Expression;
   end Set_Case_Expression;

   ---------------
   -- Transform --
   ---------------

   function Transform
     (Builder : Case_Builder'Class)
      return Leander.Core.Trees.Tree_Type
   is
      use Leander.Core, Leander.Core.Trees;
      Is_Primitive : Boolean := False;
      Is_Algebraic : Boolean := False;
      Active_Count : Natural := Builder.Pats.Last_Index;
   begin
      pragma Assert (not Builder.Pats.Is_Empty);
      pragma Assert (Builder.Pats.Last_Index = Builder.Exps.Last_Index);

      for I in 1 .. Builder.Pats.Last_Index loop
         declare
            Pat : Tree_Type renames Builder.Pats.Element (I);
         begin
            if Is_Leaf (Pat) then
               if Pat.Get_Node.Class = Variable then
                  Active_Count := I;
               elsif Pat.Get_Node.Class = Literal then
                  Is_Primitive := True;
               elsif Pat.Get_Node.Class = Constructor then
                  Is_Algebraic := True;
               end if;
            else
               if Pat.Head.Class = Constructor then
                  Is_Algebraic := True;
               else
                  raise Constraint_Error with
                    "bad pattern: " & Pat.Show;
               end if;
            end if;
         end;
      end loop;

      if not Is_Primitive and then not Is_Algebraic then
         --  must be all-variables
         declare
            Body_Expr : constant Tree_Type :=
                          Builder.Exps.First_Element;
         begin
            return Leander.Core.Trees.Apply
              (Leander.Core.Trees.Apply
                 (Leander.Core.Lambda
                      (Builder.Case_Expr.Head.Source,
                       Builder.Pats.First_Element.Get_Node.Show),
                  Body_Expr),
               Builder.Case_Expr);
         end;
      elsif Is_Primitive and then Is_Algebraic then
         Leander.Errors.Error
           (Builder.Case_Expr.Head.Source,
            "invalid type mixing in patterns");
         return Builder.Case_Expr;
      elsif Is_Algebraic then
         declare
            Result       : constant Tree_Type :=
                             Leaf
                               (Leander.Core.Algebraic_Case
                                  (Builder.Case_Expr.Head.Source));
            Alt_Map      : Algebraic_Con_Maps.Map;
            Have_Default : Boolean := False;
            Default_Pat  : Tree_Type;
            Default_Exp  : Tree_Type;
            Alts         : Tree_Type := Empty;
         begin
            for I in 1 .. Active_Count loop
               declare
                  Pat     : constant Tree_Type := Builder.Pats.Element (I);
               begin
                  if Is_Variable (Pat) then
                     pragma Assert (I = Active_Count);
                     Default_Pat := Pat;
                     Default_Exp := Builder.Exps.Element (I);
                     Have_Default := True;
                  else
                     declare
                        Head : constant String := Pat.Head.Show;
                     begin
                        if not Alt_Map.Contains (Head) then
                           Alt_Map.Insert (Head, List_Of_Con_Alts.Empty_List);
                        end if;
                        Alt_Map (Head).Append ((Pat, Builder.Exps (I)));
                     end;
                  end if;
               end;
            end loop;

            if Have_Default then
               Alts :=
                 Leander.Core.Trees.Apply
                   (Leander.Core.Trees.Apply
                      (Leander.Core.Variable
                         (Default_Pat.Head.Source,
                          Default_Pat.Head.Show),
                       Default_Exp),
                    Alts);
            end if;

            for Alt of Alt_Map loop
               Alts :=
                 Leander.Core.Trees.Apply
                   (Transform_Algebraic_Con (Alt),
                    Alts);
            end loop;

            declare
               E : constant Leander.Core.Trees.Tree_Type :=
                     Result.Apply (Builder.Case_Expr.Apply (Alts));
            begin
               return E;
            end;
         end;
      else
         declare
            Result : constant Tree_Type :=
                       Leaf
                         (Primitive_Case (Builder.Case_Expr.Head.Source));
            Alts   : Tree_Type := Empty;
         begin
            for I in reverse 1 .. Active_Count loop
               declare
                  Pat : constant Tree_Type := Builder.Pats.Element (I);
                  Alt_Pat : Tree_Type;
               begin
                  if Is_Variable (Pat) then
                     Alt_Pat :=
                       Leaf
                         (Variable
                            (Builder.Case_Expr.Head.Source,
                             Pat.Head.Show));
                  else
                     Alt_Pat := Pat;
                  end if;
                  Alts :=
                    Alt_Pat.Apply
                      (Builder.Exps (I))
                        .Apply (Alts);
               end;
            end loop;
            return Result.Apply (Builder.Case_Expr.Apply (Alts));
         end;
      end if;
   end Transform;

   -----------------------------
   -- Transform_Algebraic_Con --
   -----------------------------

   function Transform_Algebraic_Con
     (Alts : List_Of_Con_Alts.List)
      return Leander.Core.Trees.Tree_Type
   is
      use Leander.Core, Leander.Core.Trees;
      First_Pat  : constant Tree_Type :=
                     Alts.First_Element.Pat;
      Source     : constant Leander.Source.Source_Reference :=
                     First_Pat.Head.Source;
      Con_Node   : constant Core_Node := First_Pat.Head;
      Con_Name   : constant String := Con_Node.Show;
      First_Args : constant Array_Of_Trees := First_Pat.Arguments;
      Arg_Count  : constant Natural :=
                     First_Args'Length;
      Pat_Count  : constant Natural := Positive (Alts.Length);

      type Pat_Matrix_Cell is
         record
            Pat      : Tree_Type;
            Variable : Boolean;
            Done     : Boolean;
         end record;

      Pat_Matrix : array (1 .. Pat_Count, 1 .. Arg_Count) of Pat_Matrix_Cell;
      Exp_Vector : array (1 .. Pat_Count) of Tree_Type;

      type Row_Flags is array (1 .. Pat_Count) of Boolean;

      function Arg_Name (Index : Positive) return String
      is ("t" & Integer'Image (-Index));

      function Con_Expr (Row, Col : Positive) return Tree_Type;

      function Var_Expr
        (Col  : Positive;
         Rows : Row_Flags)
         return Tree_Type;

      --------------
      -- Con_Expr --
      --------------

      function Con_Expr (Row, Col : Positive) return Tree_Type is
         Result : Tree_Type := Exp_Vector (Row);
      begin
         for I in reverse Col + 1 .. Arg_Count loop
            pragma Assert (Pat_Matrix (Row, I).Variable);
            Result :=
              Apply
                (Leander.Core.Lambda
                   (Result.Head.Source,
                    Pat_Matrix (Row, I).Pat.Head.Show), Result);
         end loop;
         for I in Col + 1 .. Arg_Count loop
            Result := Apply (Result,
                             Variable (Result.Head.Source, Arg_Name (I)));
         end loop;
         return Result;
      end Con_Expr;

      --------------
      -- Var_Expr --
      --------------

      function Var_Expr
        (Col  : Positive;
         Rows : Row_Flags)
         return Tree_Type
      is
         Child_Builder : Case_Builder;
         Got_Variable : Boolean := False;
         Got_Con      : Boolean := False;
         First_Var    : Natural := 0;
         Child_Rows   : Row_Flags := (others => False);
      begin
         Child_Builder.Set_Case_Expression
           (Leaf (Variable (First_Pat.Head.Source, Arg_Name (Col))));

         for I in Pat_Matrix'Range (1) loop
            if Rows (I) then
               if not Pat_Matrix (I, Col).Variable then
                  Child_Builder.Add_Alt
                    (Pat_Matrix (I, Col).Pat,
                     Con_Expr (I, Col));
                  Got_Con := True;
               else
                  Child_Rows (I) := True;
                  Got_Variable := True;
                  if First_Var = 0 then
                     First_Var := I;
                  end if;
               end if;
            end if;
         end loop;

         if not Got_Con then
            declare
               Expr : Tree_Type :=
                        (if Col = Arg_Count
                         then Exp_Vector (First_Var)
                         else Var_Expr (Col + 1, Rows));
            begin
               Expr :=
                 Apply
                   (Lambda (Source, Pat_Matrix (First_Var, Col).Pat.Head.Show),
                    Expr);
               Expr :=
                 Apply (Expr,
                        Variable (Source, Arg_Name (Col)));
               return Expr;
            end;
         elsif Got_Variable then
            if Col < Pat_Matrix'Last (2) then
               Child_Builder.Add_Alt
                 (Leaf
                    (Variable
                         (First_Pat.Head.Source,
                          Arg_Name (Col + 1))),
                  Var_Expr (Col + 1, Child_Rows));
            else
               Child_Builder.Add_Alt
                 (Pat_Matrix (First_Var, Col).Pat,
                  Exp_Vector (First_Var));
            end if;
         end if;

         return Child_Builder.Transform;
      end Var_Expr;

   begin
      if Arg_Count = 0 then
         pragma Assert (Pat_Count = 1);
         return Leander.Core.Trees.Apply
           (First_Pat, Alts.First_Element.Exp);
      end if;

      declare
         Row : Natural := 0;
      begin
         for Alt of Alts loop
            Row := Row + 1;
            declare
               Col : Natural := 0;
            begin
               for Pat of Alt.Pat.Arguments loop
                  Col := Col + 1;
                  Pat_Matrix (Row, Col) :=
                    (Pat, Is_Variable (Pat), False);
               end loop;
               Exp_Vector (Row) := Alt.Exp;
            end;
         end loop;
      end;

      declare
         Rows         : constant Row_Flags := (others => True);
         Child_Case   : constant Tree_Type :=
                          Var_Expr (1, Rows);
         Pat          : Leander.Core.Trees.Tree_Type :=
                          Leander.Core.Trees.Leaf
                            (Leander.Core.Constructor
                               (Source => First_Pat.Head.Source,
                                Name   => Con_Name));
      begin
         for I in 1 .. Arg_Count loop
            Pat :=
              Pat.Apply
                (Leander.Core.Trees.Leaf
                   (Leander.Core.Variable
                      (First_Pat.Head.Source, Arg_Name (I))));
         end loop;
         return Pat.Apply (Child_Case);
      end;

   end Transform_Algebraic_Con;

end Leander.Core.Cases;
