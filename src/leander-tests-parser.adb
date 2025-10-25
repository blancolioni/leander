with Leander.Parser;

package body Leander.Tests.Parser is

   procedure Test (Expr     : String;
                   Expected : String);

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
   begin
      Test ("1", "1");
      Test ("1 + 2", "+ 1 2");
      Test ("1 * 2 + 3", "+ (* 1 2) 3");
      Test ("1 + 2 * 3", "* (+ 1 2) 3");  --  no precedence loaded yet
      Test ("\x -> x + 1", "\x -> + x 1");
      Test ("let x = 1 in x", "let {x=1} in x");
      Test ("let {x=1;y=2} in x + y", "let {x=1;y=2} in + x y");
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test (Expr     : String;
                   Expected : String)
   is
   begin
      Test (Expr, Expected,
            Leander.Parser.Parse_Expression (Expr).To_Core.Show);
   end Test;

end Leander.Tests.Parser;
