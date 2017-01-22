with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Fixed.Hash;

with Leander.Parser;
with Leander.Source;

with Leander.Primitives;

with Leander.Core.Cases;
with Leander.Types.Bindings;

package body Leander.Types.Instances.Derived is

   type Deriver is access
     procedure
       (Binding  : Leander.Types.Bindings.Type_Binding'Class;
        Instance : in out Leander.Types.Instances.Type_Instance'Class);

   package Derivation_Maps is
     new Ada.Containers.Indefinite_Hashed_Maps
       (Key_Type        => String,
        Element_Type    => Deriver,
        Hash            => Ada.Strings.Fixed.Hash,
        Equivalent_Keys => "=");

   Deriver_Map : Derivation_Maps.Map;

   procedure Derive_Eq
     (Binding  : Leander.Types.Bindings.Type_Binding'Class;
      Instance : in out Leander.Types.Instances.Type_Instance'Class);

   procedure Derive_Ord
     (Binding  : Leander.Types.Bindings.Type_Binding'Class;
      Instance : in out Leander.Types.Instances.Type_Instance'Class);

   procedure Derive_Show
     (Binding  : Leander.Types.Bindings.Type_Binding'Class;
      Instance : in out Leander.Types.Instances.Type_Instance'Class);

   ---------------
   -- Derive_Eq --
   ---------------

   procedure Derive_Eq
     (Binding  : Leander.Types.Bindings.Type_Binding'Class;
      Instance : in out Leander.Types.Instances.Type_Instance'Class)
   is
      use Leander.Core;
      Current : constant Leander.Source.Source_Reference :=
                  Leander.Parser.Current_Source_Reference;
      Builder : Leander.Core.Cases.Case_Builder;

      function Tuple (X, Y : Leander.Core.Core_Node)
                      return Leander.Core.Trees.Tree_Type
      is (Leander.Core.Trees.Apply
          (Leander.Core.Trees.Apply
           (Leander.Core.Constructor
            (Current,
             "(,)"),
            X),
           Y));

      function Var (Name : String) return Leander.Core.Core_Node
      is (Variable (Current, Name));

      function Con (Name : String) return Leander.Core.Core_Node
      is (Constructor (Current, Name));

   begin

      Builder.Set_Case_Expression
        (Tuple (Var ("eq-1"), Var ("eq-2")));

      for I in 1 .. Binding.Constructor_Count loop
         Builder.Add_Alt
           (Tuple
              (Con (Binding.Constructor_Name (I)),
               Con (Binding.Constructor_Name (I))),
            Leander.Core.Trees.Leaf
              (Con ("True")));
      end loop;

      Builder.Add_Alt
        (Tuple
           (Var ("_"),
            Var ("_")),
         Leander.Core.Trees.Leaf
           (Con ("False")));

      declare
         Fn : Leander.Core.Trees.Tree_Type := Builder.Transform;
      begin
         Fn :=
           Leander.Core.Trees.Apply
             (Leander.Core.Lambda (Current, "eq-2"),
              Fn);
         Fn :=
           Leander.Core.Trees.Apply
             (Leander.Core.Lambda (Current, "eq-1"),
              Fn);
         Instance.Implement ("==", Fn);
      end;
   end Derive_Eq;

   ---------------------
   -- Derive_Instance --
   ---------------------

   procedure Derive_Instance
     (Env        : Leander.Environments.Environment;
      Tree       : Leander.Types.Trees.Tree_Type;
      Class_Name : String)
   is
      Instance : Leander.Types.Instances.Type_Instance;
   begin
      if not Deriver_Map.Contains (Class_Name) then
         raise Constraint_Error with
           "invalid class for deriving: " & Class_Name;
      end if;

      Instance.Create;
      Instance.Set_Class_Assertion (Tree, Class_Name);

      Deriver_Map.Element (Class_Name)
           (Env.Type_Constructor_Binding (Tree.Head.Show), Instance);

      Env.Add_Type_Assertion
        (Tree.Head.Show, Instance);

   end Derive_Instance;

   ----------------
   -- Derive_Ord --
   ----------------

   procedure Derive_Ord
     (Binding  : Leander.Types.Bindings.Type_Binding'Class;
      Instance : in out Leander.Types.Instances.Type_Instance'Class)
   is
      use Leander.Core;
      Current : constant Leander.Source.Source_Reference :=
                  Leander.Parser.Current_Source_Reference;
      Left : Leander.Core.Cases.Case_Builder;

      function Var (Name : String) return Leander.Core.Trees.Tree_Type
      is (Leander.Core.Trees.Leaf (Variable (Current, Name)));

      function Con (Name : String) return Leander.Core.Trees.Tree_Type
      is (Leander.Core.Trees.Leaf (Constructor (Current, Name)));

   begin

      Left.Set_Case_Expression (Var ("ord-1"));

      for Left_Index in 1 .. Binding.Constructor_Count loop
         declare
            use type Leander.Types.Bindings.Constructor_Count_Range;
            Left_Con : constant String :=
                         Binding.Constructor_Name (Left_Index);
            Right : Leander.Core.Cases.Case_Builder;
         begin
            Right.Set_Case_Expression (Var ("ord-2"));
            for Right_Index in 1 .. Left_Index loop
               declare
                  Right_Con : constant String :=
                                Binding.Constructor_Name (Right_Index);
               begin
                  Right.Add_Alt
                    (Con (Right_Con),
                     Con (if Right_Index < Left_Index
                       then "GT" else "EQ"));
               end;
            end loop;
            Right.Add_Alt
              (Var ("_"), Con ("LT"));
            Left.Add_Alt (Con (Left_Con), Right.Transform);
         end;
      end loop;

      declare
         Fn : Leander.Core.Trees.Tree_Type := Left.Transform;
      begin
         Fn :=
           Leander.Core.Trees.Apply
             (Leander.Core.Lambda (Current, "ord-2"),
              Fn);
         Fn :=
           Leander.Core.Trees.Apply
             (Leander.Core.Lambda (Current, "ord-1"),
              Fn);
         Instance.Implement ("compare", Fn);
      end;

   end Derive_Ord;

   -----------------
   -- Derive_Show --
   -----------------

   procedure Derive_Show
     (Binding  : Leander.Types.Bindings.Type_Binding'Class;
      Instance : in out Leander.Types.Instances.Type_Instance'Class)
   is
      use Leander.Core, Leander.Core.Trees;

      Current : constant Leander.Source.Source_Reference :=
                  Leander.Parser.Current_Source_Reference;
      Builder : Leander.Core.Cases.Case_Builder;

      function Var (Name : String) return Leander.Core.Core_Node
      is (Variable (Current, Name));

      function Con (Name : String) return Leander.Core.Core_Node
      is (Constructor (Current, Name));

   begin

      Builder.Set_Case_Expression (Leaf (Var ("show-1")));

      for I in 1 .. Binding.Constructor_Count loop
         declare
            Con_Name : constant String :=
                         Binding.Constructor_Name (I);
            Pat      : constant Tree_Type :=
                         Leaf (Con (Con_Name));
            Exp      : Tree_Type := Leaf (Con ("[]"));
         begin
            for Ch of reverse Con_Name loop
               declare
                  Elem : constant Tree_Type :=
                           Leaf
                             (Literal
                                (Current,
                                 Natural'Image
                                   (Character'Pos (Ch))));
               begin
                  Elem.Set_Annotation (Leander.Primitives.Char_Type);
                  Exp := Apply (Apply (Con (":"), Elem), Exp);
               end;
            end loop;

            Builder.Add_Alt (Pat, Exp);
         end;
      end loop;

      declare
         Fn : Tree_Type := Builder.Transform;
      begin
         Fn := Apply (Lambda (Current, "show-1"), Fn);
         Instance.Implement ("show", Fn);
      end;
   end Derive_Show;

begin
   Deriver_Map.Insert ("Eq", Derive_Eq'Access);
   Deriver_Map.Insert ("Ord", Derive_Ord'Access);
   Deriver_Map.Insert ("Show", Derive_Show'Access);
end Leander.Types.Instances.Derived;
