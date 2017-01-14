with Leander.Environments;
with Leander.Types.Trees;

package Leander.Types.Kind_Inference is

   procedure Infer_Kinds
     (Env       : Leander.Environments.Environment);

   procedure Annotate
     (Env       : Leander.Environments.Environment;
      Root_Type : Leander.Types.Trees.Tree_Type);

end Leander.Types.Kind_Inference;
