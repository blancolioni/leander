with Leander.Kinds.Trees;
with Leander.Unifier;

package Leander.Kinds.Unifier is
  new Leander.Unifier
    (Node_Type => Kind_Node,
     Trees     => Leander.Kinds.Trees);
