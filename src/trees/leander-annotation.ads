with Leander.Annotation_Trees;
with Leander.Environments;

generic
   with package Trees is new Leander.Annotation_Trees (<>);
   with function Variable_Annotation return Trees.Annotation_Type;
   with function Map_Operator return Trees.Annotation_Type;
--     with procedure Application_Annotation
--       (Application      : Trees.Tree_Type;
--        Left, App, Right : out Trees.Annotation_Type);
package Leander.Annotation is

   procedure Annotate
     (Tree : Trees.Tree_Type;
      Env  : Leander.Environments.Environment);

--     procedure Default_Application_Annotation
--       (Application      : Trees.Tree_Type;
--        Left, App, Right : out Trees.Annotation_Type);
--
--     function Unbind
--       (Tree : Trees.Tree_Type;
--        Next : in out Positive)
--        return Trees.Tree_Type;
--
--     function Unbound_Variable return Trees.Tree_Type;

end Leander.Annotation;
