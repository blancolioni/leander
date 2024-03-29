with Ada.Strings.Unbounded;

with Leander.Core.Bindings;

package body Leander.Types.Instances is

   type Instance_Body_Record is
      record
         Constraints : Leander.Types.Trees.Tree_Type :=
                         Leander.Types.Trees.Empty;
         Type_Expr   : Leander.Types.Trees.Tree_Type;
         Class_Name  : Ada.Strings.Unbounded.Unbounded_String;
         Methods     : Leander.Core.Bindings.Binding_List;
      end record;

   --------------------
   -- Add_Constraint --
   --------------------

   procedure Add_Constraint
     (Instance   : in out Type_Instance'Class;
      Constraint : Leander.Types.Trees.Tree_Type)
   is
   begin
      Instance.Instance_Body.Constraints :=
        Constraint.Apply (Instance.Instance_Body.Constraints);
   end Add_Constraint;

   ----------------
   -- Class_Name --
   ----------------

   function Class_Name
     (Instance : Type_Instance'Class)
      return String
   is
   begin
      return Ada.Strings.Unbounded.To_String
        (Instance.Instance_Body.Class_Name);
   end Class_Name;

   ------------
   -- Create --
   ------------

   procedure Create
     (Instance  : in out Type_Instance'Class)
   is
   begin
      Instance.Instance_Body := new Instance_Body_Record;
   end Create;

   ---------------------
   -- Has_Method_Body --
   ---------------------

   function Has_Method_Body
     (Instance : Type_Instance'Class;
      Name     : String)
      return Boolean
   is
   begin
      return Instance.Instance_Body.Methods.Has_Value (Name);
   end Has_Method_Body;

   ---------------
   -- Implement --
   ---------------

   procedure Implement
     (Instance : in out Type_Instance'Class;
      Name     : String;
      Value    : Leander.Core.Trees.Tree_Type)
   is
   begin
      Instance.Instance_Body.Methods.Insert
        (Name, Value);
   end Implement;

   -----------------
   -- Method_Body --
   -----------------

   function Method_Body
     (Instance : Type_Instance'Class;
      Name     : String)
      return Leander.Core.Trees.Tree_Type
   is
   begin
      return Instance.Instance_Body.Methods.Binding (Name);
   end Method_Body;

   ----------------------
   -- Scan_Constraints --
   ----------------------

   procedure Scan_Constraints
     (Instance : Type_Instance'Class;
      Process  : not null access
        procedure (Constraint : Leander.Types.Trees.Tree_Type))
   is
      It : Leander.Types.Trees.Tree_Type := Instance.Instance_Body.Constraints;
   begin
      while not It.Is_Empty loop
         Process (It.Left);
         It := It.Right;
      end loop;
   end Scan_Constraints;

   -------------------------
   -- Set_Class_Assertion --
   -------------------------

   procedure Set_Class_Assertion
     (Instance   : in out Type_Instance'Class;
      Type_Expr  : Leander.Types.Trees.Tree_Type;
      Class_Name : String)
   is
   begin
      Instance.Instance_Body.Type_Expr := Type_Expr;
      Instance.Instance_Body.Class_Name :=
        Ada.Strings.Unbounded.To_Unbounded_String (Class_Name);
   end Set_Class_Assertion;

   ---------------
   -- Type_Tree --
   ---------------

   function Type_Tree
     (Instance : Type_Instance'Class)
      return Leander.Types.Trees.Tree_Type
   is
   begin
      return Instance.Instance_Body.Type_Expr;
   end Type_Tree;

end Leander.Types.Instances;
