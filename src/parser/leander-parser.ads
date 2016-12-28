with Leander.Environments;
with Leander.Source;
with Leander.Syntax;

package Leander.Parser is

   function Parse_Expression
     (Expr : String)
      return Leander.Syntax.Syntax_Tree;

   procedure Import_Module
     (Name : String;
      Path : String;
      Env  : in out Leander.Environments.Environment);

private

   function Current_Source_Reference return Leander.Source.Source_Reference;

   function At_Variable return Boolean;
   function At_Constructor return Boolean;

end Leander.Parser;
