with Leander.Types.Trees;

package Leander.Parser.Types is

   function At_Atomic_Type return Boolean;

   function Parse_Type
     (Env : Leander.Environments.Environment)
      return Leander.Types.Trees.Tree_Type;

   function Parse_Atomic_Type
     (Env : Leander.Environments.Environment)
      return Leander.Types.Trees.Tree_Type;

   function Parse_Type_Constructor
     (Env    : Leander.Environments.Environment;
      Target : Leander.Types.Trees.Tree_Type)
      return Leander.Types.Trees.Tree_Type;

end Leander.Parser.Types;
