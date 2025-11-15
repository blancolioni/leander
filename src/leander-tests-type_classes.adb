with Leander.Core.Kinds;
with Leander.Core.Predicates;
with Leander.Core.Types;
with Leander.Core.Tyvars;
with Leander.Environment;
with Leander.Parser;

package body Leander.Tests.Type_Classes is

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      Context : Leander.Parser.Parse_Context;
      Prelude : constant Leander.Environment.Reference :=
                  Context.Load_Module
                    ("./share/leander/modules/Prelude.hs");
      Tv_A : constant Leander.Core.Types.Reference :=
                  Leander.Core.Types.TVar
                    (Leander.Core.Tyvars.Tyvar
                       (Core.To_Varid ("a"), Leander.Core.Kinds.Star));
   begin
      Test ("entails (Eq a) (Eq a)",
            Prelude.Entails
              ([Leander.Core.Predicates.Predicate ("Eq", Tv_A)],
               Leander.Core.Predicates.Predicate ("Eq", Tv_A)));
      Test ("entails (Eq a) (Bounded a)",
            not Prelude.Entails
              ([Leander.Core.Predicates.Predicate ("Eq", Tv_A)],
               Leander.Core.Predicates.Predicate ("Bounded", Tv_A)));
      Test ("entails (Eq a, Bounded a) (Bounded a)",
            Prelude.Entails
              ([Leander.Core.Predicates.Predicate ("Eq", Tv_A),
               Leander.Core.Predicates.Predicate ("Bounded", Tv_A)],
               Leander.Core.Predicates.Predicate ("Bounded", Tv_A)));
      Test ("entails (Eq a) (Ord a)",
            Prelude.Entails
              ([Leander.Core.Predicates.Predicate ("Eq", Tv_A)],
               Leander.Core.Predicates.Predicate ("Ord", Tv_A)));
      Test ("entails () (Eq Int)",
            Prelude.Entails
              ([],
               Leander.Core.Predicates.Predicate ("Eq", Core.Types.T_Int)));
   end Run_Tests;

end Leander.Tests.Type_Classes;
