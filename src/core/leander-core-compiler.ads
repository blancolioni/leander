with SK.Machine;

with Leander.Core.Trees;
with Leander.Environments;

package Leander.Core.Compiler is

   procedure Compile
     (Env     : Leander.Environments.Environment;
      Name    : String;
      Tree    : Leander.Core.Trees.Tree_Type;
      Machine : SK.Machine.SK_Machine);

   procedure Compile_Instance_Method
     (Env           : Leander.Environments.Environment;
      Name          : String;
      Instance_Name : String;
      Tree          : Leander.Core.Trees.Tree_Type;
      Machine       : SK.Machine.SK_Machine);

end Leander.Core.Compiler;
