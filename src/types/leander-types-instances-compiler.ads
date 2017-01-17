with SK.Machine;
with Leander.Environments;

package Leander.Types.Instances.Compiler is

   procedure Compile
     (Env      : Leander.Environments.Environment;
      Instance : Type_Instance'Class;
      Machine  : SK.Machine.SK_Machine);

end Leander.Types.Instances.Compiler;
