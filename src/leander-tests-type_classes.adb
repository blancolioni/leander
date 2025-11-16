with Leander.Core;
with Leander.Core.Kinds;
with Leander.Core.Predicates;
with Leander.Core.Type_Classes;
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
      Tv_A    : constant Leander.Core.Types.Reference :=
                  Leander.Core.Types.TVar
                    (Leander.Core.Tyvars.Tyvar
                       (Core.To_Varid ("a"), Leander.Core.Kinds.Star));

      function Check_Reduce
        (Input       : Leander.Core.Predicates.Predicate_Array;
         Expected    : Leander.Core.Predicates.Predicate_Array)
         return Boolean;

      ------------------
      -- Check_Reduce --
      ------------------

      function Check_Reduce
        (Input       : Leander.Core.Predicates.Predicate_Array;
         Expected    : Leander.Core.Predicates.Predicate_Array)
         return Boolean
      is
         Success : Boolean;
         Result  : constant Leander.Core.Predicates.Predicate_Array :=
                     Prelude.Reduce (Input, Success);
      begin
         return Success and then
                Leander.Core.Predicates.Show (Result) =
                  Leander.Core.Predicates.Show (Expected);
      end Check_Reduce;

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
      Test ("entails [Ord a] Eq a",
            Prelude.Entails
              ([Leander.Core.Predicates.Predicate ("Ord", Tv_A)],
               Leander.Core.Predicates.Predicate ("Eq", Tv_A)));
      Test ("entails () (Eq Int)",
            Prelude.Entails
              ([],
               Leander.Core.Predicates.Predicate ("Eq", Core.Types.T_Int)));
      Test ("to-hnf Eq a == [Eq a]",
            Prelude.To_Head_Normal_Form
              (Leander.Core.Predicates.Predicate ("Eq", Tv_A))'Length = 1);
      Test ("to-hnf Eq [a] == [Eq a]",
            Prelude.To_Head_Normal_Form
              (Leander.Core.Predicates.Predicate ("Eq", Leander.Core.Types.List_Of (Tv_A))) (1).Show = "Eq a");
      Test ("to-hnf Eq Int == []",
            Leander.Core.Predicates.Show
              (Prelude.To_Head_Normal_Form
                (Leander.Core.Predicates.Predicate ("Eq", Leander.Core.Types.T_Int))) = "");
      Test ("reduce [Eq a, Eq a, Ord a] = [Ord a]",
            Check_Reduce
              ([Leander.Core.Predicates.Predicate ("Eq", Tv_A),
                Leander.Core.Predicates.Predicate ("Eq", Tv_A),
                Leander.Core.Predicates.Predicate ("Ord", Tv_A)],
               [Leander.Core.Predicates.Predicate ("Ord", Tv_A)]));

   end Run_Tests;

end Leander.Tests.Type_Classes;
