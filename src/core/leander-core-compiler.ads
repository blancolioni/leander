with SK.Machine;

with Leander.Core.Trees;
with Leander.Environments;

package Leander.Core.Compiler is

   procedure Compile
     (Env     : Leander.Environments.Environment;
      Name    : String;
      Tree    : Leander.Core.Trees.Tree_Type;
      Machine : SK.Machine.SK_Machine);

end Leander.Core.Compiler;
