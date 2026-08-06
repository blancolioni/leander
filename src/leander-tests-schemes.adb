with Ada.Streams;

with Leander.Core.Kinds;
with Leander.Core.Predicates;
with Leander.Core.Schemes.Serialize;
with Leander.Core.Types;
with Leander.Core.Tyvars;

package body Leander.Tests.Schemes is

   package Kinds renames Leander.Core.Kinds;
   package Predicates renames Leander.Core.Predicates;
   package Schemes renames Leander.Core.Schemes;
   package Types renames Leander.Core.Types;
   package Tyvars renames Leander.Core.Tyvars;

   procedure Test
     (Name : String;
      S    : Schemes.Reference);
   --  Round-trip S through Encode/Decode and check the decoded scheme shows
   --  the same as the original (Show renders TGen by index, so this is a
   --  faithful check of the encoding, not just of predicate/kind counts).

   ---------------
   -- Run_Tests --
   ---------------

   procedure Run_Tests is
      function TVar (Name : String; K : Kinds.Kind := Kinds.Star)
        return Types.Reference
      is (Types.TVar (Tyvars.Tyvar (Leander.Core.To_Varid (Name), K)));
   begin
      Test ("monomorphic", Schemes.To_Scheme (Types.T_Int));

      Test ("polymorphic, no predicates",
            Schemes.Quantify
              ([Tyvars.Tyvar (Leander.Core.To_Varid ("a"), Kinds.Star)],
               [],
               Types.Fn (TVar ("a"), TVar ("a"))));

      Test ("polymorphic, one predicate",
            Schemes.Quantify
              ([Tyvars.Tyvar (Leander.Core.To_Varid ("a"), Kinds.Star)],
               [Predicates.Predicate ("Eq", TVar ("a"))],
               Types.Fn (TVar ("a"), Types.Fn (TVar ("a"), Types.T_Int))));

      declare
         Fun_Kind : constant Kinds.Kind :=
                      Kinds.Kind_Function (Kinds.Star, Kinds.Star);
      begin
         Test ("higher-kinded quantified variable",
               Schemes.Quantify
                 ([Tyvars.Tyvar (Leander.Core.To_Varid ("f"), Fun_Kind),
                   Tyvars.Tyvar (Leander.Core.To_Varid ("a"), Kinds.Star)],
                  [],
                  Types.Fn
                    (Types.Application (TVar ("f"), TVar ("a")),
                     Types.Application (TVar ("f"), TVar ("a")))));
      end;
   end Run_Tests;

   ----------
   -- Test --
   ----------

   procedure Test
     (Name : String;
      S    : Schemes.Reference)
   is
      Bytes   : constant Ada.Streams.Stream_Element_Array :=
                  Leander.Core.Schemes.Serialize.Encode (S);
      Decoded : constant Schemes.Reference :=
                  Leander.Core.Schemes.Serialize.Decode (Bytes);
   begin
      Test (Name, S.Show, Decoded.Show);
   end Test;

end Leander.Tests.Schemes;
