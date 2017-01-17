with Leander.Types.Class_Constraints;
with Leander.Core.Bindings;
with Leander.Core.Trees;
with Leander.Types.Trees;

package Leander.Types.Instances is

   type Type_Instance is new Type_Assertion with private;

   procedure Create
     (Instance  : in out Type_Instance'Class);

   procedure Set_Class_Assertion
     (Instance   : in out Type_Instance'Class;
      Type_Expr  : Leander.Types.Trees.Tree_Type;
      Class_Name : String);

   procedure Add_Constraint
     (Instance   : in out Type_Instance'Class;
      Constraint : Type_Constraint'Class);

   procedure Implement
     (Instance : in out Type_Instance'Class;
      Name     : String;
      Value    : Leander.Core.Trees.Tree_Type);

   function Class_Name
     (Instance : Type_Instance'Class)
      return String;

   function Type_Tree
     (Instance : Type_Instance'Class)
      return Leander.Types.Trees.Tree_Type;

   function Has_Method_Body
     (Instance : Type_Instance'Class;
      Name     : String)
      return Boolean;

   function Method_Body
     (Instance : Type_Instance'Class;
      Name     : String)
      return Leander.Core.Trees.Tree_Type
     with Pre => Instance.Has_Method_Body (Name);

private

   type Instance_Body_Record;

   type Type_Instance is new Type_Assertion with
      record
         Instance_Body : access Instance_Body_Record;
      end record;

end Leander.Types.Instances;
