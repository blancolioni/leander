with Leander.Trees;
with Leander.Unifiable;

generic
   type Node_Type is
     new Leander.Unifiable.Unifiable_Node_Interface
     and Show_Interface
   with private;
   with package Trees is
     new Leander.Trees (Node_Type, <>, <>);
package Leander.Unifier is

   procedure Unify
     (Left, Right : Trees.Tree_Type;
      Success     : out Boolean;
      Result      : out Trees.Tree_Type);

end Leander.Unifier;
