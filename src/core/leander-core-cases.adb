with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Strings.Fixed.Hash;

with Leander.Errors;
with Leander.Primitives;

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

   package Con_Sets is
     new Ada.Containers.Indefinite_Hashed_Sets
       (Element_Type        => String,
        Hash                => Ada.Strings.Fixed.Hash,
        Equivalent_Elements => "=");

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
--        Leander.Logging.Log
--          ("  " & Pattern.Show & " -> " & Expression.Show);

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
--        Leander.Logging.Log ("case " & Expression.Show & " of");
      Builder.Case_Expr := Expression;
   end Set_Case_Expression;

   ---------------------------
   -- Simple_Algebraic_Case --
   ---------------------------

   function Simple_Algebraic_Case
     (Builder : Case_Builder'Class)
      return Boolean
   is
      use Leander.Core.Trees;
      First_Pat        : constant Tree_Type := Builder.Pats.First_Element;
      First_Args       : constant Array_Of_Trees := First_Pat.Arguments;
      Con              : constant String := First_Pat.Head.Show;
      Variable_Columns : array (First_Args'Range) of Tree_Type :=
                           (others => Empty);
      Variable_Count   : Natural := 0;

      function Simple_Pattern (Pat : Tree_Type) return Boolean;

      --------------------
      -- Simple_Pattern --
      --------------------

      function Simple_Pattern (Pat : Tree_Type) return Boolean is
      begin
         if Pat.Is_Variable or else
           (Pat.Is_Leaf and then Pat.Get_Node.Class = Literal)
         then
            return False;
         end if;

         declare
            Args : constant Array_Of_Trees := Pat.Arguments;
         begin
            for Arg of Args loop
               if not Arg.Is_Variable then
                  return False;
               end if;
            end loop;
         end;

         return True;
      end Simple_Pattern;

   begin
      for I in First_Args'Range loop
         if First_Args (I).Is_Variable then
            Variable_Columns (I) := First_Args (I);
            Variable_Count := Variable_Count + 1;
         elsif not Simple_Pattern (First_Args (I)) then
            return False;
         end if;
      end loop;

      if Variable_Count < First_Args'Length - 1 then
         return False;
      end if;

      for Pat of Builder.Pats loop
         if Pat.Head.Class = Literal then
            return False;
         end if;
         if Pat.Head.Show /= Con then
            return False;
         end if;

         declare
            Pat_Args : constant Array_Of_Trees := Pat.Arguments;
         begin
            if Pat_Args'Length /= First_Args'Length then
               return False;
            end if;

            for I in Pat_Args'Range loop
               if Variable_Columns (I).Is_Empty then
                  if not Simple_Pattern (Pat_Args (I)) then
                     return False;
                  end if;
               elsif not Pat_Args (I).Is_Variable then
                  return False;
               else
                  if Pat_Args (I).Show
                    /= Variable_Columns (I).Show
                  then
                     return False;
                  end if;
               end if;
            end loop;
         end;
      end loop;

      return True;

   end Simple_Algebraic_Case;

   -----------------------------
   -- Simple_Constructor_Case --
   -----------------------------

   function Simple_Constructor_Case
     (Builder : Case_Builder'Class)
      return Boolean
   is
      use Trees;
   begin
      if Builder.Case_Expr.Head.Class /= Constructor
        or else Builder.Pats.Last_Index /= 1
      then
         return False;
      end if;

      declare
         Con_Name : constant String := Builder.Case_Expr.Head.Show;
      begin
         if Builder.Pats.Element (1).Head.Class /= Constructor
           or else Builder.Pats.Element (1).Head.Show /= Con_Name
         then
            return False;
         end if;
      end;

      declare
         Exp_Args : constant Array_Of_Trees := Builder.Case_Expr.Arguments;
         Pat_Args : constant Array_Of_Trees := Builder.Pats (1).Arguments;
      begin
         if Exp_Args'Length /= Pat_Args'Length then
            return False;
         end if;

         for I in Exp_Args'Range loop
            if not Exp_Args (I).Is_Leaf
              or else not Pat_Args (I).Is_Leaf
              or else not Exp_Args (I).Is_Variable
              or else not Pat_Args (I).Is_Variable
            then
               return False;
            end if;
         end loop;
      end;

      return True;
   end Simple_Constructor_Case;

   ---------------
   -- Transform --
   ---------------

   function Transform
     (Builder : Case_Builder'Class)
      return Leander.Core.Trees.Tree_Type
   is
      use Leander.Core.Trees;
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
      elsif Builder.Trivial_Constructor_Case then
--           Leander.Logging.Log
--             ("trivial constructor case: "
--                & Builder.Exps (1).Show);
         return Builder.Exps (1);
      elsif Builder.Simple_Constructor_Case then
         declare
            Exp_Args : constant Array_Of_Trees := Builder.Case_Expr.Arguments;
            Pat_Args : constant Array_Of_Trees := Builder.Pats (1).Arguments;
            Result   : Tree_Type := Builder.Exps (1);
         begin
            for I in reverse Pat_Args'Range loop
               Result :=
                 Apply
                   (Lambda
                      (Pat_Args (I).Get_Node.Source,
                       Pat_Args (I).Get_Node.Show),
                    Result);
            end loop;
            for I in Exp_Args'Range loop
               Result :=
                 Apply (Result, Exp_Args (I).Get_Node);
            end loop;

--              Leander.Logging.Log
--                ("simple constructor case: "
--                 & Result.Show);
            return Result;
         end;

      elsif Is_Algebraic then
         if Builder.Simple_Algebraic_Case then
            return Builder.Transform_Simple_Algebraic_Case;
         else
            return Builder.Transform_General_Algebraic_Case;
         end if;
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

      function Con_Expr (Row, Col : Positive) return Tree_Type
        with Unreferenced;

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
--           Got_Variable  : Boolean := False;
         Got_Con       : Boolean := False;
         First_Var     : Natural := 0;
         Col_Cons      : Con_Sets.Set;
         Col_Vars      : Con_Sets.Set;
      begin
         Child_Builder.Set_Case_Expression
           (Leaf (Variable (Source, Arg_Name (Col))));

         for I in Pat_Matrix'Range (1) loop
            if Rows (I) then
               if Pat_Matrix (I, Col).Variable then
                  declare
                     Var_Name : constant String :=
                                  Pat_Matrix (I, Col).Pat.Show;
                  begin
                     if not Col_Vars.Contains (Var_Name) then
                        Col_Vars.Insert (Var_Name);
                     end if;
                     if First_Var = 0 then
                        First_Var := I;
                     end if;
                  end;
               else
                  declare
                     Con_Name : constant String :=
                                  Pat_Matrix (I, Col).Pat.Head.Show;
                  begin
                     if not Col_Cons.Contains (Con_Name) then
                        Col_Cons.Insert (Con_Name);
                     end if;
                     Got_Con := True;
                  end;
               end if;
            end if;
         end loop;

         for Con_Name of Col_Cons loop
            declare
               Exp      : Tree_Type :=
                            Leaf
                              (if Col < Arg_Count - 1
                               then Constructor
                                 (Source,
                                  Leander.Primitives.Tuple_Name
                                    (Arg_Count - Col))
                               else
                                  Variable
                                 (Source,
                                  Arg_Name (Col + 1)));
               Con_Case   : Case_Builder;
               Child_Rows : Row_Flags := (others => False);
               Child_Pat  : Tree_Type := Empty;
            begin
               if Col < Arg_Count - 1 then
                  for I in Col + 1 .. Arg_Count loop
                     Exp :=
                       Apply
                         (Exp,
                          Variable
                            (Source, Arg_Name (I)));
                  end loop;
               end if;

               Con_Case.Set_Case_Expression (Exp);

               for I in Pat_Matrix'Range (1) loop
                  if Pat_Matrix (I, Col).Variable
                    or else Pat_Matrix (I, Col).Pat.Head.Show = Con_Name
                  then
                     Child_Rows (I) := True;

                     if Child_Pat.Is_Empty then
                        Child_Pat :=
                          Leaf (Constructor (Source, Con_Name));
                        for J in Pat_Matrix (I, Col).Pat.Arguments'Range loop
                           Child_Pat :=
                             Apply (Child_Pat,
                                    Variable (Source, Arg_Name (J)));
                        end loop;
                     end if;
                  end if;
               end loop;

               for I in Pat_Matrix'Range (1) loop
                  if Child_Rows (I) then
                     declare
                        Pat      : Tree_Type :=
                                     (if Col < Arg_Count - 1
                                      then Trees.Leaf
                                        (Constructor
                                           (Source,
                                            Leander.Primitives.Tuple_Name
                                              (Arg_Count - Col)))
                                         else Pat_Matrix (I, Arg_Count).Pat);
                     begin
                        if Col < Arg_Count - 1 then
                           for I in Col + 1 .. Arg_Count loop
                              Pat :=
                                Apply
                                  (Pat,
                                   Variable
                                     (Source,
                                      Arg_Name (I)));
                           end loop;
                        end if;

                        if Col < Arg_Count - 1 then
                           Con_Case.Add_Alt
                             (Pat, Var_Expr (Col + 1, Child_Rows));
                        else
                           Con_Case.Add_Alt
                             (Pat, Exp_Vector (I));
                        end if;
                     end;
                  end if;
               end loop;

               Child_Builder.Add_Alt
                 (Child_Pat, Con_Case.Transform);
            end;

         end loop;

         if not Got_Con then
            if Col = Arg_Count - 1 then
               declare
                  Child_Builder : Case_Builder;
               begin
                  Child_Builder.Set_Case_Expression
                    (Leaf (Variable (Source, Arg_Name (Arg_Count))));
                  for I in Rows'Range loop
                     if Rows (I) then
                        Child_Builder.Add_Alt
                          (Pat_Matrix (I, Arg_Count).Pat,
                           Exp_Vector (I));
                     end if;
                  end loop;

                  declare
                     Result : Tree_Type := Child_Builder.Transform;
                  begin
                     Result :=
                       Apply
                         (Lambda
                            (Source,
                             Pat_Matrix (First_Var, Col).Pat.Show),
                          Result);
                     Result :=
                       Apply (Result, Variable (Source, Arg_Name (Col)));
                     return Result;
                  end;
               end;
            else
               declare
                  Expr : Tree_Type :=
                           (if Col = Arg_Count
                            then Exp_Vector (First_Var)
                            else Var_Expr (Col + 1, Rows));
               begin
                  Expr :=
                    Apply
                      (Lambda
                         (Source, Pat_Matrix (First_Var, Col).Pat.Head.Show),
                       Expr);
                  Expr :=
                    Apply (Expr,
                           Variable (Source, Arg_Name (Col)));
                  return Expr;
               end;
            end if;
--           elsif Got_Variable then
--              if Col < Pat_Matrix'Last (2) then
--                 Child_Builder.Add_Alt
--                   (Leaf
--                      (Variable
--                           (First_Pat.Head.Source,
--                            Arg_Name (Col + 1))),
--                    Var_Expr (Col + 1, Child_Rows));
--              else
--                 Child_Builder.Add_Alt
--                   (Pat_Matrix (First_Var, Col).Pat,
--                    Exp_Vector (First_Var));
--              end if;
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

   --------------------------------------
   -- Transform_General_Algebraic_Case --
   --------------------------------------

   function Transform_General_Algebraic_Case
     (Builder : Case_Builder'Class)
      return Leander.Core.Trees.Tree_Type
   is
      use Trees;
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
      for I in 1 .. Builder.Pats.Last_Index loop
         declare
            Pat     : constant Tree_Type := Builder.Pats.Element (I);
         begin
            if Is_Variable (Pat) then
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
   end Transform_General_Algebraic_Case;

   -------------------------------------
   -- Transform_Simple_Algebraic_Case --
   -------------------------------------

   function Transform_Simple_Algebraic_Case
     (Builder : Case_Builder'Class)
      return Leander.Core.Trees.Tree_Type
   is
      use Leander.Core.Trees;
      pragma Assert (Builder.Simple_Algebraic_Case);

      Source : constant Leander.Source.Source_Reference :=
                 Builder.Case_Expr.Head.Source;
      First_Pat        : constant Tree_Type := Builder.Pats.First_Element;
      Con_Name         : constant String := First_Pat.Head.Show;
      First_Args       : constant Array_Of_Trees := First_Pat.Arguments;
      Column_Count     : constant Natural := First_Args'Length;
      Variable_Columns : array (First_Args'Range) of Tree_Type :=
                           (others => Empty);
      Variable_Count   : Natural := 0;
      Pattern_Column   : Natural := 0;
   begin
      for I in First_Args'Range loop
         if First_Args (I).Is_Variable then
            Variable_Columns (I) := First_Args (I);
            Variable_Count := Variable_Count + 1;
         else
            Pattern_Column := I;
         end if;
      end loop;

      pragma Assert
        ((Variable_Count = Column_Count and then Pattern_Column = 0)
         or else (Variable_Count = Column_Count - 1
           and then Pattern_Column > 0));

      declare
         Expr : Tree_Type := Empty;
      begin
         if Pattern_Column = 0 then
            for I in reverse 1 .. Builder.Pats.Last_Index loop
               Expr :=
                 Apply
                   (Apply
                      (Builder.Pats (I), Builder.Exps (I)),
                    Expr);
            end loop;
            Expr := Apply (Builder.Case_Expr, Expr);
            Expr := Apply
              (Algebraic_Case (Builder.Case_Expr.Head.Source), Expr);

         else
            declare
               Child_Case : Case_Builder;
               Pat_Var     : constant Core_Node :=
                               New_Variable (Source);
               Parent_Pat  : Tree_Type :=
                               Leaf (Constructor (Source, Con_Name));
            begin
               for I in First_Args'Range loop
                  if Variable_Columns (I).Is_Empty then
                     Parent_Pat :=
                       Apply (Parent_Pat, Leaf (Pat_Var));
                  else
                     Parent_Pat :=
                       Apply (Parent_Pat, Variable_Columns (I));
                  end if;
               end loop;

               Child_Case.Set_Case_Expression (Leaf (Pat_Var));

               for I in 1 .. Builder.Pats.Last_Index loop
                  Child_Case.Add_Alt
                    (Pattern    =>
                       Builder.Pats.Element (I).Arguments (Pattern_Column),
                     Expression =>
                       Builder.Exps.Element (I));
               end loop;

               Expr := Child_Case.Transform;

               declare
                  Parent_Case : Case_Builder;
               begin
                  Parent_Case.Set_Case_Expression (Builder.Case_Expr);
                  Parent_Case.Add_Alt (Parent_Pat, Expr);
                  Expr := Parent_Case.Transform;
               end;

            end;

         end if;

         return Expr;
      end;
   end Transform_Simple_Algebraic_Case;

   ------------------
   -- Trivial_Case --
   ------------------

   function Trivial_Case
     (Builder : Case_Builder'Class)
      return Boolean
   is
   begin
      return Builder.Pats.Last_Index = 1
        and then Builder.Pats.First_Element.Is_Variable;
   end Trivial_Case;

   ------------------------------
   -- Trivial_Constructor_Case --
   ------------------------------

   function Trivial_Constructor_Case
     (Builder : Case_Builder'Class)
      return Boolean
   is
   begin
      if not Builder.Simple_Constructor_Case then
         return False;
      end if;

      declare
         use Trees;
         Exp_Args : constant Array_Of_Trees := Builder.Case_Expr.Arguments;
         Pat_Args : constant Array_Of_Trees := Builder.Pats (1).Arguments;
      begin
         for I in Exp_Args'Range loop
            if Exp_Args (I).Show /= Pat_Args (I).Show then
               return False;
            end if;
         end loop;
      end;

      return True;
   end Trivial_Constructor_Case;

end Leander.Core.Cases;
