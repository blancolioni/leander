with SK.Machine;

with Leander.Environments;

package Leander.Types.Class_Constraints.Compiler is

   procedure Compile
     (Binding : Class_Constraint'Class;
      Env     : Leander.Environments.Environment;
      Machine : SK.Machine.SK_Machine);

end Leander.Types.Class_Constraints.Compiler;
