with Leander.Syntax.Expressions.Constructors;
package body Leander.Syntax.Expressions.Applications is

   ----------------
   -- To_Pattern --
   ----------------

   overriding function To_Pattern
     (This : Instance)
      return Leander.Syntax.Patterns.Reference
   is
      function Go (Expr         : Expressions.Instance'Class;
                   Current_Args : Leander.Syntax.Patterns.Reference_Array)
                   return Leander.Syntax.Patterns.Reference;

      function Go (Expr         : Expressions.Instance'Class;
                   Current_Args : Leander.Syntax.Patterns.Reference_Array)
                   return Leander.Syntax.Patterns.Reference
      is
      begin
         if Expr in Expressions.Constructors.Instance'Class then
            return Leander.Syntax.Patterns.Constructor
              (Expr.Location,
               Core.To_String
                 (Expressions.Constructors.Instance'Class (Expr).Conid),
               Current_Args);
         elsif Expr in Expressions.Applications.Instance'Class then
            declare
               use type Syntax.Patterns.Reference_Array;
               App : Expressions.Applications.Instance'Class renames
                       Expressions.Applications.Instance'Class (Expr);
            begin
               return Go (App.Left.all, App.Right.To_Pattern & Current_Args);
            end;
         else
            raise Constraint_Error with "invalid pattern";
         end if;
      end Go;

   begin
      return Go (This, []);
   end To_Pattern;

end Leander.Syntax.Expressions.Applications;
