private with Ada.Strings.Unbounded;

private with Leander.Kinds.Primitives;
private with Leander.Kinds.Trees;

with Leander.Source;

with Leander.Types.Trees;
with Leander.Unifiable;

package Leander.Core is

   type Core_Node is
     new Leander.Unifiable.Unifiable_Interface
     and Show_Interface
   with private;

   function Core_Type
     (Core : Core_Node)
      return Leander.Types.Trees.Tree_Type;

   function Constructor
     (Source   : Leander.Source.Source_Reference;
      Name     : String)
      return Core_Node;

   function Variable
     (Source   : Leander.Source.Source_Reference;
      Name     : String)
      return Core_Node;

   function Literal
     (Source     : Leander.Source.Source_Reference;
      Text       : String;
      Annotation : Leander.Types.Trees.Tree_Type)
      return Core_Node;

   function Lambda
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Core_Node;

   function Algebraic_Case
     (Source : Leander.Source.Source_Reference)
      return Core_Node;

   function Primitive_Case
     (Source : Leander.Source.Source_Reference)
      return Core_Node;

   function Variable_Type
     return Leander.Types.Trees.Tree_Type;

   function Map_Operator
     return Leander.Types.Trees.Tree_Type;

   function Has_Annotation
     (Node : Core_Node'Class)
      return Boolean;

   function Annotation
     (Node : Core_Node'Class)
      return Leander.Types.Trees.Tree_Type;

   function Source
     (Node : Core_Node'Class)
      return Leander.Source.Source_Reference;

private

   function "+"
     (Item : String)
      return Ada.Strings.Unbounded.Unbounded_String
      renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-"
     (Item : Ada.Strings.Unbounded.Unbounded_String)
      return String
      renames Ada.Strings.Unbounded.To_String;

   type Core_Node_Class is
     (Algebraic_Case, Primitive_Case, Lambda,
      Constructor, Literal, Variable);

   subtype Primitive_Node_Class is
     Core_Node_Class range Constructor .. Variable;

   type Core_Node is
     new Leander.Unifiable.Unifiable_Interface
     and Show_Interface with
      record
         Source     : Leander.Source.Source_Reference;
         Class      : Core_Node_Class;
         Core_Type  : Leander.Types.Trees.Tree_Type :=
                        Leander.Types.Trees.Empty;
         Name       : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   overriding function Show
     (Node : Core_Node)
      return String;

   overriding function Is_Variable
     (Node : Core_Node)
      return Boolean
   is (Node.Class = Variable);

   overriding function Is_Binding
     (Node : Core_Node)
      return Boolean
   is (False);

   overriding function Is_Constructor
     (Node : Core_Node)
      return Boolean
   is (Node.Class = Constructor);

   overriding function Variable_Name
     (Node : Core_Node)
      return String
   is (Node.Show);

   overriding function Constructor_Name
     (Node : Core_Node)
      return String
   is (Node.Show);

   overriding function Binding_Index
     (Node : Core_Node)
      return Positive
   is (1);

   overriding function Create_Variable_From_Binding
     (Node  : Core_Node;
      Index : Positive)
      return Core_Node
   is (Node);

   overriding procedure Set_Binding_Index
     (Node  : in out Core_Node;
      Index : Positive)
   is null;

   function Core_Type
     (Core : Core_Node)
      return Leander.Types.Trees.Tree_Type
   is (Core.Core_Type);

   function Constructor
     (Source   : Leander.Source.Source_Reference;
      Name     : String)
      return Core_Node
   is (Class => Constructor,
       Name => +Name,
       Source => Source,
       others => <>);

   function Literal
     (Source     : Leander.Source.Source_Reference;
      Text       : String;
      Annotation : Leander.Types.Trees.Tree_Type)
      return Core_Node
   is (Class => Literal,
       Name => +Text,
       Source => Source,
       Core_Type => Annotation);

   function Lambda
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Core_Node
   is (Class => Lambda, Source => Source, Name => +Name, others => <>);

   function Algebraic_Case
     (Source : Leander.Source.Source_Reference)
      return Core_Node
   is (Class => Algebraic_Case, Source => Source, others => <>);

   function Primitive_Case
     (Source : Leander.Source.Source_Reference)
      return Core_Node
   is (Class => Primitive_Case, Source => Source, others => <>);

   function Variable
     (Source : Leander.Source.Source_Reference;
      Name   : String)
      return Core_Node
   is (Class => Variable, Name => +Name, Source => Source, others => <>);

   function Variable_Type
     return Leander.Types.Trees.Tree_Type
   is (Leander.Types.Trees.Leaf
         (Leander.Types.Variable
          ("a", Leander.Kinds.Trees.Leaf (Leander.Kinds.Primitive))));

   function Map_Operator
     return Leander.Types.Trees.Tree_Type
   is (Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
        ("->", Leander.Kinds.Primitives.Type_Con_2)));

   function Has_Annotation
     (Node : Core_Node'Class)
      return Boolean
   is (not Node.Core_Type.Is_Empty);

   function Annotation
     (Node : Core_Node'Class)
      return Leander.Types.Trees.Tree_Type
   is (Node.Core_Type);

   function Source
     (Node : Core_Node'Class)
      return Leander.Source.Source_Reference
   is (Node.Source);

end Leander.Core;
