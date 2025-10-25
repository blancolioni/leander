with Leander.Core.Inference;

package Leander.Core.Binding_Groups.Inference is

   procedure Infer
     (Context       : in out Leander.Core.Inference.Inference_Context'Class;
      Binding_Group : not null access constant Instance'Class);

end Leander.Core.Binding_Groups.Inference;
