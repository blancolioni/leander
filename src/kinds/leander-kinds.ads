with Leander.Trees;
with Leander.Unifiable;

package Leander.Kinds is

   type Kind_Node_Class is (Binding, Variable, Primitive, Map);

   type Kind_Node is
     new Leander.Unifiable.Unifiable_Interface
     and Show_Interface
   with private;

   function Variable (Name : Character) return Kind_Node;
   function Primitive return Kind_Node;
   function Map return Kind_Node;
   function Binding (Index : Positive) return Kind_Node;

   type Kind_Annotation is
     new Leander.Trees.Tree_Interface
     and Leander.Unifiable.Unifiable_Interface with private;

   function Variable_Kind_Annotation return Kind_Annotation;

   function Map_Operator return Kind_Annotation;

   function Has_Annotation (Kind : Kind_Node'Class) return Boolean;
   function Annotation (Kind : Kind_Node'Class) return Kind_Annotation;

private

   type Kind_Annotation is
     new Leander.Trees.Tree_Interface
     and Leander.Unifiable.Unifiable_Interface with null record;

   overriding function Show (Annotation : Kind_Annotation) return String
   is ("+");

   overriding function Is_Empty (Annotation : Kind_Annotation) return Boolean
   is (False);

   overriding function Is_Leaf (Annotation : Kind_Annotation) return Boolean
   is (True);

   overriding function Is_Application
     (Annotation : Kind_Annotation)
      return Boolean
   is (False);

   overriding function Is_Binding
     (Annotation : Kind_Annotation)
      return Boolean
   is (False);

   overriding function Is_Variable
     (Annotation : Kind_Annotation)
      return Boolean
   is (False);

   overriding function Is_Constructor
     (Annotation : Kind_Annotation)
      return Boolean
   is (True);

   overriding function Binding_Index
     (Annotation : Kind_Annotation)
      return Positive
   is (1);

   overriding procedure Set_Binding_Index
     (Annotation : in out Kind_Annotation;
      Index      : Positive)
   is null;

   overriding function Empty
      return Kind_Annotation
   is (null record);

   overriding function Create_Variable_From_Binding
     (Node  : Kind_Annotation;
      Index : Positive)
      return Kind_Annotation
   is (Node);

   overriding function Variable_Name
     (Annotation : Kind_Annotation)
      return String
   is ("");

   overriding function Constructor_Name
     (Annotation : Kind_Annotation)
      return String
   is ("+");

   overriding function Left
     (Annotation : Kind_Annotation)
      return Kind_Annotation
   is (Annotation);

   overriding function Right
     (Annotation : Kind_Annotation)
      return Kind_Annotation
   is (Annotation);

   overriding function Apply
     (Left, Right : Kind_Annotation)
      return Kind_Annotation
   is (Left);

   type Kind_Node is
     new Leander.Unifiable.Unifiable_Interface
     and Show_Interface with
      record
         Class : Kind_Node_Class;
         Name  : Character;
         Index : Positive;
      end record;

   overriding function Show
     (Node : Kind_Node)
      return String
   is (case Node.Class is
          when Variable =>
             (1 => Node.Name),
          when Primitive =>
             "*",
          when Binding =>
             "_",
          when Map =>
             "->");

   overriding function Is_Binding
     (Node : Kind_Node)
      return Boolean
   is (Node.Class = Binding);

   overriding function Is_Variable
     (Node : Kind_Node)
      return Boolean
   is (Node.Class = Variable);

   overriding function Is_Constructor
     (Node : Kind_Node)
      return Boolean
   is (Node.Class /= Variable);

   overriding function Variable_Name
     (Node : Kind_Node)
      return String
   is (1 => Node.Name);

   overriding function Binding_Index
     (Node : Kind_Node)
      return Positive
   is (Node.Index);

   overriding function Constructor_Name
     (Node : Kind_Node)
      return String
   is (Node.Show);

   overriding function Create_Variable_From_Binding
     (Node  : Kind_Node;
      Index : Positive)
      return Kind_Node
   is (Variable, Character'Val (Character'Pos ('a') + Index - 1), 1);

   overriding procedure Set_Binding_Index
     (Node  : in out Kind_Node;
      Index : Positive);

   function Has_Annotation (Kind : Kind_Node'Class) return Boolean
   is (True);

   function Annotation (Kind : Kind_Node'Class) return Kind_Annotation
   is (null record);

   function Variable (Name : Character) return Kind_Node
   is (Variable, Name, 1);

   function Binding (Index : Positive) return Kind_Node
   is (Binding, '_', Index);

   function Primitive return Kind_Node is
     (Primitive, '*', 1);

   function Map return Kind_Node
   is (Map, '>', 1);

   function Variable_Kind_Annotation return Kind_Annotation
   is (null record);

   function Map_Operator return Kind_Annotation
   is (null record);

end Leander.Kinds;
