with Leander.Environments;
with Leander.Core.Trees;

package Leander.Core.Lets is

   function Let_Expression
     (Bindings   : Leander.Environments.Environment;
      Expression : Leander.Core.Trees.Tree_Type)
      return Leander.Core.Trees.Tree_Type;

end Leander.Core.Lets;
