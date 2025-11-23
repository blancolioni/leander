package body Leander.Core.Predicates is

   ----------
   -- Show --
   ----------

   function Show (Ps : Predicate_Array) return String is

      function Shw (Index : Positive) return String;

      ---------
      -- Shw --
      ---------

      function Shw (Index : Positive) return String is
      begin
         if Index < Ps'Last then
            return Ps (Index).Show & "," & Shw (Index + 1);
         else
            return Ps (Index).Show;
         end if;
      end Shw;

   begin
      case Ps'Length is
         when 0 =>
            return "";
         when 1 =>
            return Ps (Ps'First).Show;
         when others =>
            return "(" & Shw (Ps'First) & ")";
      end case;
   end Show;

end Leander.Core.Predicates;