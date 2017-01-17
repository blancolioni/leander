with Leander.Parser;
with Leander.Source;

with Leander.Core.Cases;
with Leander.Types.Bindings;

package body Leander.Types.Instances.Derived is

   procedure Derive_Eq
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
      Instance.Create;
      Instance.Set_Class_Assertion (Tree, Class_Name);

      if Class_Name = "Eq" then
         Derive_Eq
           (Env.Type_Constructor_Binding (Tree.Head.Show), Instance);
      else
         raise Constraint_Error with
           "invalid class for deriving: " & Class_Name;
      end if;

      Env.Add_Type_Assertion
        (Tree.Head.Show, Instance);

   end Derive_Instance;

end Leander.Types.Instances.Derived;
