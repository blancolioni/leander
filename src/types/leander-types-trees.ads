with Leander.Annotation_Trees;

package Leander.Types.Trees is

   package Type_Trees is
     new Leander.Annotation_Trees
       (Type_Node, Leander.Kinds.Trees.Tree_Type, Has_Annotation, Annotation);

   subtype Tree_Type is Type_Trees.Tree_Type;

   function "=" (Left, Right : Tree_Type) return Boolean
                 renames Type_Trees."=";

   function Empty return Tree_Type renames Type_Trees.Empty;

   function Leaf (Node : Type_Node) return Tree_Type
                  renames Type_Trees.Leaf;

   function Apply (Left, Right : Tree_Type) return Tree_Type
                   renames Type_Trees.Apply;

   function Apply (Left, Right : Type_Node) return Tree_Type
                   renames Type_Trees.Apply;

   function Apply (Left  : Type_Node;
                   Right : Tree_Type)
                   return Tree_Type
                   renames Type_Trees.Apply;

   function Apply (Left  : Tree_Type;
                   Right : Type_Node)
                   return Tree_Type
                   renames Type_Trees.Apply;

end Leander.Types.Trees;
