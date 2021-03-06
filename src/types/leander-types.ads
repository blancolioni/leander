private with Ada.Containers.Indefinite_Doubly_Linked_Lists;
private with Ada.Strings.Unbounded;

with Leander.Kinds.Trees;
with Leander.Unifiable;

package Leander.Types is

   type Type_Assertion is interface;

   type Type_Constraint is interface and Show_Interface;

   function Is_Subset_Of
     (Subset   : Type_Constraint;
      Superset : Type_Constraint)
      return Boolean
      is abstract;

   type Type_Node is
     new Leander.Unifiable.Unifiable_Interface
     and Show_Interface
   with private;

   overriding function "=" (Left, Right : Type_Node) return Boolean;

   function Constructor
     (Name : String)
      return Type_Node;

   function Variable
     (Name : String)
      return Type_Node;

   function Binding
     (Index : Positive)
      return Type_Node;

   function Variable_Kind return Leander.Kinds.Trees.Tree_Type;
   function Map_Operator return Leander.Kinds.Trees.Tree_Type;

   overriding procedure Merge_Constraints
     (Left, Right : in out Type_Node);

   procedure Add_Constraint
     (Node       : in out Type_Node;
      Constraint : Type_Constraint'Class);

   procedure Add_Assertion
     (Node      : in out Type_Node;
      Assertion : Type_Assertion'Class);

   procedure Scan_Assertions
     (Node    : Type_Node'Class;
      Process : not null access
        procedure (Assertion : Type_Assertion'Class));

private

   package Assertion_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Type_Assertion'Class);

   package Constraint_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
       (Type_Constraint'Class);

   function Show_Constraints (List : Constraint_Lists.List) return String;

   procedure Minimise
     (List : in out Constraint_Lists.List);

   type Type_Node_Class is (Binding, Constructor, Variable);

   type Type_Node is
     new Leander.Unifiable.Unifiable_Interface
     and Show_Interface with
      record
         Class       : Type_Node_Class;
         Name        : Ada.Strings.Unbounded.Unbounded_String;
         Index       : Natural := 0;
         Assertions  : Assertion_Lists.List;
         Constraints : Constraint_Lists.List;
      end record;

   overriding function Show
     (Node : Type_Node)
      return String
   is (if Node.Class = Binding
       then Natural'Image (Node.Index)
       else Ada.Strings.Unbounded.To_String (Node.Name));

   overriding function Is_Variable
     (Node : Type_Node)
      return Boolean
   is (Node.Class = Variable);

   overriding function Is_Primitive
     (Node : Type_Node)
      return Boolean
   is (False);

   overriding function Is_Binding
     (Node : Type_Node)
      return Boolean
   is (Node.Class = Binding);

   overriding function Is_Constructor
     (Node : Type_Node)
      return Boolean
   is (Node.Class = Constructor);

   overriding function Variable_Name
     (Node : Type_Node)
      return String
   is (Node.Show);

   overriding function Constructor_Name
     (Node : Type_Node)
      return String
   is (Node.Show);

   overriding function Binding_Index
     (Node : Type_Node)
      return Positive
   is (Node.Index);

   overriding function Create_Variable_From_Binding
     (Node  : Type_Node;
      Index : Positive)
      return Type_Node;

   overriding procedure Set_Binding_Index
     (Node  : in out Type_Node;
      Index : Positive);

--     function Has_Annotation (Item : Type_Node'Class) return Boolean
--     is (not Item.Kind.Is_Empty);
--
--     function Annotation
--       (Item : Type_Node'Class)
--        return Leander.Kinds.Trees.Tree_Type
--     is (Item.Kind);

   function Binding
     (Index : Positive)
      return Type_Node
   is (Class => Binding,
       Index => Index,
       others => <>);

   function Constructor
     (Name : String)
      return Type_Node
   is (Constructor,
       Ada.Strings.Unbounded.To_Unbounded_String (Name),
       others => <>);

   function Variable
     (Name : String)
      return Type_Node
   is (Variable,
       Ada.Strings.Unbounded.To_Unbounded_String (Name),
       others => <>);

   function Variable_Kind return Leander.Kinds.Trees.Tree_Type
   is (Leander.Kinds.Trees.Leaf (Leander.Kinds.Variable ('a')));

   function Map_Operator return Leander.Kinds.Trees.Tree_Type
   is (Leander.Kinds.Trees.Leaf (Leander.Kinds.Map));

end Leander.Types;
