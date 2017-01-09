with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed.Hash;
with Ada.Strings.Unbounded;

with Leander.Errors;

package body Leander.Syntax.Expressions is

   function "+"
     (Item : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-"
     (Item : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   type Literal_Node is
     new Expression_Node with
      record
         Primitive_Text : Ada.Strings.Unbounded.Unbounded_String;
         Primitive_Type : Leander.Types.Trees.Tree_Type;
      end record;

   overriding function Show
     (Node : Literal_Node)
      return String
   is (Ada.Strings.Unbounded.To_String (Node.Primitive_Text));

   overriding function Transform
     (Node   : Literal_Node)
      return Leander.Core.Trees.Tree_Type;

   function Is_Literal (Tree : Syntax_Tree) return Boolean
   is (Tree.Is_Expression
       and then Tree.Get_Expression.all in Literal_Node'Class);

   type Named_Node is
     abstract new Expression_Node with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Show
     (Node : Named_Node)
      return String
   is (Ada.Strings.Unbounded.To_String (Node.Name));

   type Constructor_Node is
     new Named_Node with null record;

   overriding function Transform
     (Node   : Constructor_Node)
      return Leander.Core.Trees.Tree_Type;

   function Is_Constructor (Tree : Syntax_Tree) return Boolean
   is (Tree.Is_Expression
       and then Tree.Get_Expression.all in Constructor_Node'Class);

   type Variable_Node is
     new Named_Node with null record;

   overriding function Transform
     (Node   : Variable_Node)
      return Leander.Core.Trees.Tree_Type;

   function Is_Variable (Tree : Syntax_Tree) return Boolean
   is (Tree.Is_Expression
       and then Tree.Get_Expression.all in Variable_Node'Class);

   type Application_Node is
     new Expression_Node with
      record
         Left, Right : Syntax_Tree_Record;
      end record;

   overriding function Show
     (Node : Application_Node)
      return String;

   overriding function Transform
     (Node   : Application_Node)
      return Leander.Core.Trees.Tree_Type
   is (Node.Left.Get_Expression.Transform.Apply
       (Node.Right.Get_Expression.Transform));

   overriding function Has_Left (Node : Application_Node) return Boolean
   is (True);

   overriding function Left_Child (Node : Application_Node) return Syntax_Tree
   is (Node.Left);

   type Lambda_Node is
     new Expression_Node with
      record
         Name : Ada.Strings.Unbounded.Unbounded_String;
         Expr : Syntax_Tree_Record;
      end record;

   overriding function Show
     (Node : Lambda_Node)
      return String
   is ("\" & (-Node.Name) & "->" & Node.Expr.Show);

   overriding function Transform
     (Node   : Lambda_Node)
      return Leander.Core.Trees.Tree_Type;

   package Syntax_Tree_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Syntax_Tree);

   function Show_Alts (Pats, Exps : Syntax_Tree_Vectors.Vector;
                       Start      : Positive)
                       return String;

   type Algebraic_Alt is
      record
         Pat : Syntax_Tree_Record;
         Exp : Syntax_Tree_Record;
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

   type Case_Node is
     new Expression_Node with
      record
         Expr : Syntax_Tree_Record;
         Pats : Syntax_Tree_Vectors.Vector;
         Exps : Syntax_Tree_Vectors.Vector;
      end record;

   overriding function Show
     (Node : Case_Node)
      return String
   is ("case " & Node.Expr.Show & " of "
       & Show_Alts (Node.Pats, Node.Exps, 1));

   overriding function Transform
     (Node   : Case_Node)
      return Leander.Core.Trees.Tree_Type;

   ------------------------
   -- Add_Case_Alternate --
   ------------------------

   procedure Add_Case_Alternate
     (Case_Expr  : Syntax_Tree'Class;
      Pattern    : Syntax_Tree'Class;
      Expression : Syntax_Tree'Class)
   is
      Node : Case_Node renames Case_Node (Case_Expr.Node.all);
   begin
      Node.Pats.Append (Pattern);
      Node.Exps.Append (Expression);
   end Add_Case_Alternate;

   ---------------------------
   -- Application_Arguments --
   ---------------------------

   function Application_Arguments
     (Tree : Syntax_Tree)
      return Array_Of_Syntax_Trees
   is
   begin
      if Tree.Get_Expression.all not in Application_Node'Class then
         return Empty_Tree_Constant;
      else
         declare
            App : Application_Node renames
                    Application_Node (Tree.Get_Expression.all);
         begin
            return Application_Arguments (App.Left) & App.Right;
         end;
      end if;
   end Application_Arguments;

   -----------
   -- Apply --
   -----------

   function Apply
     (Source      : Leander.Source.Source_Reference;
      Left, Right : Syntax_Tree'Class)
      return Syntax_Tree
   is
      Node : constant Application_Node :=
               (Source => Source,
                Left   => Syntax_Tree_Record (Left),
                Right  => Syntax_Tree_Record (Right));
   begin
      return Create (Node);
   end Apply;

   ---------------------
   -- Case_Expression --
   ---------------------

   function Case_Expression
     (Source     : Leander.Source.Source_Reference;
      Expression : Syntax_Tree'Class)
      return Syntax_Tree
   is
      Node : constant Case_Node :=
               (Source => Source,
                Expr   => Syntax_Tree_Record (Expression),
                others => <>);
   begin
      return Create (Node);
   end Case_Expression;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Syntax_Tree
   is
      Node : constant Constructor_Node :=
               (Source => Source,
                Name   => Ada.Strings.Unbounded.To_Unbounded_String (Name));
   begin
      return Create (Node);
   end Constructor;

   ------------
   -- Lambda --
   ------------

   function Lambda
     (Source   : Leander.Source.Source_Reference;
      Variable : String;
      Expr     : Syntax_Tree'Class)
      return Syntax_Tree
   is
      Node : constant Lambda_Node :=
               (Source => Source,
                Name   => +Variable,
                Expr   => Syntax_Tree_Record (Expr));
   begin
      return Create (Node);
   end Lambda;

   -------------
   -- Literal --
   -------------

   function Literal
     (Source       : Leander.Source.Source_Reference;
      Text         : String;
      Literal_Type : Leander.Types.Trees.Tree_Type)
      return Syntax_Tree'Class
   is
      Node : constant Literal_Node :=
               (Source         => Source,
                Primitive_Text =>
                  Ada.Strings.Unbounded.To_Unbounded_String (Text),
                Primitive_Type => Literal_Type);
   begin
      return Create (Node);
   end Literal;

   ----------
   -- Name --
   ----------

   function Name
     (Tree : Syntax_Tree)
      return String
   is
   begin
      if Is_Constructor (Tree) then
         return -Constructor_Node (Tree.Get_Expression.all).Name;
      elsif Is_Literal (Tree) then
         return -Literal_Node (Tree.Get_Expression.all).Primitive_Text;
      elsif Is_Variable (Tree) then
         return -Variable_Node (Tree.Get_Expression.all).Name;
      else
         raise Program_Error with
           "expected constructor, literal or variable";
      end if;
   end Name;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Node : Application_Node)
      return String
   is
   begin
      if Node.Right.Node.all in Application_Node'Class then
         return Node.Left.Show & " (" & Node.Right.Show & ")";
      else
         return Node.Left.Show & " " & Node.Right.Show;
      end if;
   end Show;

   ---------------
   -- Show_Alts --
   ---------------

   function Show_Alts (Pats, Exps : Syntax_Tree_Vectors.Vector;
                       Start      : Positive)
                       return String
   is
   begin
      if Start > Pats.Last_Index then
         return "";
      else
         return "(" & Pats (Start).Show & "->" & Exps (Start).Show & ")"
           & Show_Alts (Pats, Exps, Start + 1);
      end if;
   end Show_Alts;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Node   : Literal_Node)
      return Leander.Core.Trees.Tree_Type
   is
   begin
      return Core : constant Leander.Core.Trees.Tree_Type :=
        Leander.Core.Trees.Leaf
          (Leander.Core.Literal
             (Node.Source,
              Ada.Strings.Unbounded.To_String (Node.Primitive_Text)))
      do
         Core.Set_Annotation (Node.Primitive_Type);
      end return;
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Node   : Variable_Node)
      return Leander.Core.Trees.Tree_Type
   is
   begin
      return
        Leander.Core.Trees.Leaf
          (Leander.Core.Variable
             (Node.Source, Ada.Strings.Unbounded.To_String (Node.Name)));
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Node   : Constructor_Node)
      return Leander.Core.Trees.Tree_Type
   is
   begin
      return
        Leander.Core.Trees.Leaf
          (Leander.Core.Constructor
             (Node.Source, Ada.Strings.Unbounded.To_String (Node.Name)));
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Node   : Lambda_Node)
      return Leander.Core.Trees.Tree_Type
   is
   begin
      return Leander.Core.Trees.Apply
        (Leander.Core.Lambda (Node.Source, -Node.Name),
         Node.Expr.Get_Expression.Transform);
   end Transform;

   ---------------
   -- Transform --
   ---------------

   overriding function Transform
     (Node   : Case_Node)
      return Leander.Core.Trees.Tree_Type
   is
      Case_Expr : constant Leander.Core.Trees.Tree_Type :=
                    Node.Expr.Get_Expression.Transform;
      Is_Primitive : Boolean := False;
      Is_Algebraic : Boolean := False;
      Active_Count : Natural := Node.Pats.Last_Index;
   begin
      pragma Assert (not Node.Pats.Is_Empty);
      pragma Assert (Node.Pats.Last_Index = Node.Exps.Last_Index);

      for I in 1 .. Node.Pats.Last_Index loop
         declare
            Pat : Syntax_Tree renames Node.Pats.Element (I);
         begin
            if Is_Variable (Pat) then
               Active_Count := I;
            elsif Is_Literal (Pat) then
               Is_Primitive := True;
            elsif Is_Constructor (Pat.Left_Most) then
               Is_Algebraic := True;
            else
               raise Constraint_Error with
                 "bad pattern: " & Pat.Show;
            end if;
         end;
      end loop;

      if not Is_Primitive and then not Is_Algebraic then
         --  must be all-variables
         declare
            Body_Expr : constant Leander.Core.Trees.Tree_Type :=
                          Node.Exps.First_Element.Get_Expression.Transform;
         begin
            return Leander.Core.Trees.Apply
              (Leander.Core.Trees.Apply
                 (Leander.Core.Lambda
                      (Node.Source,
                       -(Variable_Node
                         (Node.Pats.First_Element.Node.all).Name)),
                  Body_Expr),
               Case_Expr);
         end;
      elsif Is_Primitive and then Is_Algebraic then
         Leander.Errors.Error
           (Node.Source, "invalid type mixing in patterns");
         return Case_Expr;
      elsif Is_Algebraic then
         declare
            use Leander.Core.Trees;
            Result       : constant Tree_Type :=
                             Leaf
                               (Leander.Core.Algebraic_Case
                                  (Node.Source));
            Alt_Map      : Algebraic_Con_Maps.Map;
            Have_Default : Boolean := False;
            Default_Pat  : Syntax_Tree_Record;
            Default_Exp  : Syntax_Tree_Record;
            Alts         : Tree_Type := Empty;
         begin
            for I in 1 .. Active_Count loop
               declare
                  Pat     : constant Syntax_Tree := Node.Pats.Element (I);
               begin
                  if Is_Variable (Pat) then
                     pragma Assert (I = Active_Count);
                     Default_Pat := Syntax_Tree_Record (Pat);
                     Default_Exp := Syntax_Tree_Record (Node.Exps.Element (I));
                     Have_Default := True;
                  else
                     declare
                        Head : constant String :=
                                 Pat.Left_Most.Get_Expression.Show;
                     begin
                        if not Alt_Map.Contains (Head) then
                           Alt_Map.Insert (Head, List_Of_Con_Alts.Empty_List);
                        end if;
                        Alt_Map (Head).Append
                          ((Syntax_Tree_Record (Pat),
                           Syntax_Tree_Record (Node.Exps.Element (I))));
                     end;
                  end if;
               end;
            end loop;

            if Have_Default then
               Alts :=
                 Leander.Core.Trees.Apply
                   (Leander.Core.Trees.Apply
                      (Leander.Core.Variable
                         (Default_Pat.Source,
                          Default_Pat.Show),
                       Default_Exp.Get_Expression.Transform),
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
                     Result.Apply (Case_Expr.Apply (Alts));
            begin
               return E;
            end;
         end;
      else
         declare
            use Leander.Core, Leander.Core.Trees;
            Result : constant Tree_Type :=
                       Leaf
                         (Primitive_Case (Node.Source));
            Alts   : Tree_Type := Empty;
         begin
            for I in reverse 1 .. Active_Count loop
               declare
                  Pat : constant Syntax_Tree := Node.Pats.Element (I);
                  Alt_Pat : Tree_Type;
               begin
                  if Is_Variable (Pat) then
                     Alt_Pat :=
                       Leaf
                         (Variable
                            (Pat.Node.Source,
                             -(Variable_Node (Pat.Node.all).Name)));
                  else
                     Alt_Pat := Pat.Get_Expression.Transform;
                  end if;
                  Alts :=
                    Alt_Pat.Apply
                      (Node.Exps.Element (I).Get_Expression.Transform)
                        .Apply (Alts);
               end;
            end loop;
            return Result.Apply (Case_Expr.Apply (Alts));
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
      First_Pat  : constant Syntax_Tree :=
                     Alts.First_Element.Pat;
      Source     : constant Leander.Source.Source_Reference :=
                     First_Pat.Source;
      Con_Node   : constant Constructor_Node :=
                     Constructor_Node
                       (First_Pat.Left_Most.Get_Expression.all);
      Con_Name   : constant String :=
                     -Con_Node.Name;
      First_Args : constant Array_Of_Syntax_Trees :=
                     Application_Arguments (First_Pat);
      Arg_Count  : constant Natural :=
                     First_Args'Length;
      Pat_Count  : constant Natural := Positive (Alts.Length);

      type Pat_Matrix_Cell is
         record
            Pat : Syntax_Tree_Record;
            Variable : Boolean;
            Done     : Boolean;
         end record;

      Pat_Matrix : array (1 .. Pat_Count, 1 .. Arg_Count) of Pat_Matrix_Cell;
      Exp_Vector : array (1 .. Pat_Count) of Syntax_Tree_Record;

      type Row_Flags is array (1 .. Pat_Count) of Boolean;

      function Arg_Name (Index : Positive) return String
      is ("t" & Integer'Image (-Index));

      function Con_Expr (Row, Col : Positive) return Syntax_Tree;

      function Var_Expr
        (Col  : Positive;
         Rows : Row_Flags)
         return Syntax_Tree;

      --------------
      -- Con_Expr --
      --------------

      function Con_Expr (Row, Col : Positive) return Syntax_Tree is
         Result : Syntax_Tree_Record := Exp_Vector (Row);
      begin
         for I in reverse Col + 1 .. Arg_Count loop
            pragma Assert (Pat_Matrix (Row, I).Variable);
            Result :=
              Syntax_Tree_Record
                (Lambda (Result.Source, Pat_Matrix (Row, I).Pat.Show, Result));
         end loop;
         for I in Col + 1 .. Arg_Count loop
            Result :=
              Syntax_Tree_Record
                (Apply
                   (Result.Source, Result,
                    Variable (Result.Source, Arg_Name (I))));
         end loop;
         return Result;
      end Con_Expr;

      --------------
      -- Var_Expr --
      --------------

      function Var_Expr
        (Col  : Positive;
         Rows : Row_Flags)
         return Syntax_Tree
      is
         Child_Case   : constant Syntax_Tree :=
                          Case_Expression (Source,
                                           Variable
                                             (First_Pat.Node.Source,
                                              Arg_Name (Col)));
         Got_Variable : Boolean := False;
         Got_Con      : Boolean := False;
         First_Var    : Natural := 0;
         Child_Rows   : Row_Flags := (others => False);
      begin
         for I in Pat_Matrix'Range (1) loop
            if Rows (I) then
               if not Pat_Matrix (I, Col).Variable then
                  Add_Case_Alternate
                    (Child_Case, Pat_Matrix (I, Col).Pat,
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
               Expr : Syntax_Tree :=
                        (if Col = Arg_Count
                         then Exp_Vector (First_Var)
                         else Var_Expr (Col + 1, Rows));
            begin
               Expr := Lambda (Source, Pat_Matrix (First_Var, Col).Pat.Show,
                               Expr);
               Expr := Apply (Source, Expr, Variable (Source, Arg_Name (Col)));
               return Expr;
            end;
         elsif Got_Variable then
            if Col < Pat_Matrix'Last (2) then
               Add_Case_Alternate
                 (Child_Case,
                  Variable
                    (First_Pat.Node.Source,
                     Arg_Name (Col + 1)),
                  Var_Expr (Col + 1, Child_Rows));
            else
               Add_Case_Alternate
                 (Child_Case,
                  Pat_Matrix (First_Var, Col).Pat,
                  Exp_Vector (First_Var));
            end if;
         end if;

         return Child_Case;
      end Var_Expr;

   begin
      if Arg_Count = 0 then
         pragma Assert (Pat_Count = 1);
         return Leander.Core.Trees.Apply
           (First_Pat.Get_Expression.Transform,
            Alts.First_Element.Exp.Get_Expression.Transform);
      end if;

      declare
         Row : Natural := 0;
      begin
         for Alt of Alts loop
            Row := Row + 1;
            declare
               Col : Natural := 0;
            begin
               for Pat of Application_Arguments (Alt.Pat) loop
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
         Child_Case   : constant Syntax_Tree :=
                          Var_Expr (1, Rows);
         Pat          : Leander.Core.Trees.Tree_Type :=
                          Leander.Core.Trees.Leaf
                            (Leander.Core.Constructor
                               (Source => First_Pat.Source,
                                Name   => Con_Name));
      begin
         for I in 1 .. Arg_Count loop
            Pat :=
              Pat.Apply
                (Leander.Core.Trees.Leaf
                   (Leander.Core.Variable
                      (First_Pat.Source, Arg_Name (I))));
         end loop;
         return Pat.Apply (Child_Case.Get_Expression.Transform);
      end;

   end Transform_Algebraic_Con;

   --------------
   -- Variable --
   --------------

   function Variable
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Syntax_Tree
   is
      Node : constant Variable_Node :=
               (Source => Source,
                Name   => Ada.Strings.Unbounded.To_Unbounded_String (Name));
   begin
      return Create (Node);
   end Variable;

end Leander.Syntax.Expressions;
