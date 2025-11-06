with Leander.Core.Inference;

package Leander.Core.Patterns.Inference is

   procedure Infer
     (Context : in out Leander.Core.Inference.Inference_Context'Class;
      Pattern : not null access constant Instance'Class);

end Leander.Core.Patterns.Inference;
