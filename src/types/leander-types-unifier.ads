with Leander.Types.Trees;
with Leander.Unifier;

package Leander.Types.Unifier is
  new Leander.Unifier
    (Node_Type => Type_Node,
     Trees     => Leander.Types.Trees);
