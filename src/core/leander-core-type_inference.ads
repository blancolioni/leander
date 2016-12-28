with Leander.Core.Bindings;
with Leander.Types.Bindings;
with Leander.Environments;

package Leander.Core.Type_Inference is

   procedure Infer_Types
     (Bindings : Leander.Core.Bindings.Binding_List;
      Types    : Leander.Types.Bindings.Type_Binding_List;
      Cons     : Leander.Types.Bindings.Constructor_Binding_List;
      Env      : Leander.Environments.Environment);

end Leander.Core.Type_Inference;
