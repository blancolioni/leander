with Leander.Types.Kind_Inference;

with Leander.Logging;

package body Leander.Types.Class_Constraints.Annotation is

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Binding : Class_Constraint'Class;
      Env     : Leander.Environments.Environment)
   is
      Class_Env : Leander.Environments.Environment;

      procedure Annotate_Method
        (Name      : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type);

      ---------------------
      -- Annotate_Method --
      ---------------------

      procedure Annotate_Method
        (Name      : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type)
      is
         pragma Unreferenced (Name);
         pragma Unreferenced (Default);
      begin
         Leander.Types.Kind_Inference.Annotate (Class_Env, Signature);
      end Annotate_Method;

   begin
      Class_Env.Create_Temporary_Environment
        (Env, Binding.Name);
      Class_Env.Insert_Type_Variable
        (Binding.Type_Variable.Variable_Name,
         Binding.Type_Variable);
      Binding.Scan_Methods (Annotate_Method'Access);

      Leander.Logging.Log
        (Binding.Show & " " & Binding.Type_Variable.Show_With_Annotations);
   end Annotate;

end Leander.Types.Class_Constraints.Annotation;
