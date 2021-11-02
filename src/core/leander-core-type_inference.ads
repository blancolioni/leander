with Leander.Core.Trees;
with Leander.Environments;

package Leander.Core.Type_Inference is

   procedure Infer_Types
     (Env      : Leander.Environments.Environment);

   procedure Annotate
     (Tree     : Leander.Core.Trees.Tree_Type;
      Env      : Leander.Environments.Environment);

end Leander.Core.Type_Inference;
