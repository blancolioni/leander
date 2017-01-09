with Ada.Containers.Vectors;
with Ada.Text_IO;

with Leander.Core;
with Leander.Kinds.Trees;

package body Leander.Primitives is

   Local_Int_Type   : Leander.Types.Trees.Tree_Type;
   Local_List_Type  : Leander.Types.Trees.Tree_Type;
   Local_Empty_List : Leander.Types.Trees.Tree_Type;
   Local_Cons       : Leander.Types.Trees.Tree_Type;
   Local_Map_Type   : Leander.Types.Trees.Tree_Type;

   type Tuple_Entry_Record is
      record
         Type_Con  : Leander.Types.Trees.Tree_Type :=
                       Leander.Types.Trees.Empty;
         Value_Con : Leander.Types.Trees.Tree_Type :=
                       Leander.Types.Trees.Empty;
      end record;

   package Tuple_Vectors is
     new Ada.Containers.Vectors (Positive, Tuple_Entry_Record);

   Tuples : Tuple_Vectors.Vector;

   procedure Check_Tuple (Arity : Positive);

   -----------------
   -- Check_Tuple --
   -----------------

   procedure Check_Tuple (Arity : Positive) is
   begin
      while Tuples.Last_Index < Arity loop
         Tuples.Append ((others => <>));
      end loop;
      if Tuples.Element (Arity).Type_Con.Is_Empty then
         declare
            Name  : constant String := Tuple_Name (Arity);
            Tycon : Leander.Types.Trees.Tree_Type;
            Con   : Leander.Types.Trees.Tree_Type;
            Kind  : Leander.Kinds.Trees.Tree_Type :=
                      Leander.Kinds.Trees.Leaf
                        (Leander.Kinds.Primitive);
         begin
            for I in 1 .. Arity loop
               Kind :=
                 Leander.Kinds.Trees.Apply
                   (Leander.Kinds.Trees.Apply
                      (Leander.Kinds.Map,
                       Leander.Kinds.Primitive),
                    Kind);
            end loop;

            Tycon :=
              Leander.Types.Trees.Leaf
                (Leander.Types.Constructor (Name));
            Tycon.Set_Annotation (Kind);

--              declare
--                 It : Leander.Kinds.Trees.Tree_Type := Kind;
--              begin
--                 for I in 1 .. Arity loop
--                    Tycon :=
--                      Tycon.Apply
--                        (Leander.Types.Variable
--                           ((1 => Character'Val (96 + I)),
--                            Leander.Kinds.Trees.Leaf
--                              (Leander.Kinds.Primitive)));
--                    Tycon.Set_Annotation
--                      (It.Right);
--                    It := It.Right;
--                 end loop;
--              end;

            Con := Tycon;
            for I in 1 .. Arity loop
               Con :=
                 Leander.Types.Trees.Apply
                   (Con,
                    Leander.Types.Variable
                      ((1 => Character'Val (96 + I))));
               Con.Right.Set_Annotation
                 (Leander.Kinds.Trees.Leaf
                    (Leander.Kinds.Primitive));
            end loop;
            for I in reverse 1 .. Arity loop
               Con :=
                 Leander.Types.Trees.Apply
                   (Leander.Types.Trees.Apply
                      (Leander.Core.Map_Operator,
                       Leander.Types.Variable
                         ((1 => Character'Val (96 + I)))),
                    Con);
               Con.Right.Set_Annotation
                 (Leander.Kinds.Trees.Leaf
                    (Leander.Kinds.Primitive));
            end loop;

            Ada.Text_IO.Put_Line
              (Tycon.Show & " :: " & Tycon.Annotation.Show);
            Ada.Text_IO.Put_Line
              (Con.Show);

            Tuples (Arity) :=
              Tuple_Entry_Record'(Type_Con  => Tycon,
                                  Value_Con => Con);
         end;
      end if;
   end Check_Tuple;

   ----------
   -- Cons --
   ----------

   function Cons return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Cons;
   end Cons;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Empty_List;
   end Empty_List;

   --------------
   -- Int_Type --
   --------------

   function Int_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Int_Type;
   end Int_Type;

   ---------------
   -- List_Type --
   ---------------

   function List_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_List_Type;
   end List_Type;

   --------------
   -- Map_Type --
   --------------

   function Map_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Map_Type;
   end Map_Type;

   ---------------
   -- Tuple_Con --
   ---------------

   function Tuple_Con
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type
   is
   begin
      Check_Tuple (Arity);
      return Tuples.Element (Arity).Value_Con;
   end Tuple_Con;

   ----------------
   -- Tuple_Name --
   ----------------

   function Tuple_Name
     (Arity : Positive)
      return String
   is
   begin
      return S : String (1 .. Arity + 1) := (others => ',') do
         S (S'First) := '(';
         S (S'Last) := ')';
      end return;
   end Tuple_Name;

   ----------------
   -- Tuple_Type --
   ----------------

   function Tuple_Type
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type
   is
   begin
      Check_Tuple (Arity);
      return Tuples.Element (Arity).Type_Con;
   end Tuple_Type;

   Type_Con_0  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Leaf
                     (Leander.Kinds.Primitive);
   Type_Con_1  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_0);
   Type_Con_2  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_1);

   Type_Variable_A : constant Leander.Types.Trees.Tree_Type :=
                       Leander.Types.Trees.Leaf
                         (Leander.Types.Variable ("a"));
begin
   Type_Variable_A.Set_Annotation (Type_Con_0);

   Local_Int_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
          ("Int"));
   Local_Int_Type.Set_Annotation (Type_Con_0);

   Local_List_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor ("[]"));
   Local_List_Type.First_Leaf.Set_Annotation (Type_Con_1);

   Local_Map_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor ("->"));
   Local_Map_Type.First_Leaf.Set_Annotation (Type_Con_2);

   Local_Empty_List :=
     Leander.Types.Trees.Apply
       (Local_List_Type, Type_Variable_A);

   Local_Cons :=
     Leander.Types.Trees.Apply
       (Leander.Types.Trees.Apply
          (Leander.Types.Constructor ("->"), Type_Variable_A),
        Leander.Types.Trees.Apply
          (Local_List_Type, Type_Variable_A));

end Leander.Primitives;
