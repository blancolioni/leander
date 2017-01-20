package Leander.Unifiable is

   pragma Pure (Leander.Unifiable);

   type Unifiable_Interface is interface;

   function Is_Constructor
     (Node : Unifiable_Interface) return Boolean
      is abstract;

   function Is_Primitive
     (Node : Unifiable_Interface) return Boolean
      is abstract;

   function Is_Variable
     (Node : Unifiable_Interface) return Boolean
      is abstract;

   function Is_Binding
     (Node : Unifiable_Interface) return Boolean
      is abstract;

   function Binding_Index
     (Node : Unifiable_Interface)
      return Positive
      is abstract
     with Pre'Class => Unifiable_Interface'Class (Node).Is_Binding;

   function Variable_Name
     (Node : Unifiable_Interface)
      return String
      is abstract
     with Pre'Class => Unifiable_Interface'Class (Node).Is_Variable;

   function Constructor_Name
     (Node : Unifiable_Interface)
      return String
      is abstract
     with Pre'Class => Unifiable_Interface'Class (Node).Is_Constructor;

   procedure Set_Binding_Index
     (Node  : in out Unifiable_Interface;
      Index : Positive)
   is abstract;

   procedure Merge_Constraints
     (Left, Right : in out Unifiable_Interface)
   is null;

   function Create_Variable_From_Binding
     (Node  : Unifiable_Interface;
      Index : Positive)
      return Unifiable_Interface
      is abstract
     with Pre'Class => Unifiable_Interface'Class (Node).Is_Binding,
     Post'Class => Unifiable_Interface'Class
       (Create_Variable_From_Binding'Result).Is_Variable;

end Leander.Unifiable;
