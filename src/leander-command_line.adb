with Ada.Command_Line;

with WL.Command_Line;

package body Leander.Command_Line is

   function Enable_Log return Boolean
   is (WL.Command_Line.Find_Option ("enable-log", ' '));

   function Report return Boolean
   is (WL.Command_Line.Find_Option ("report", ' '));

   function Self_Test return Boolean
   is (WL.Command_Line.Find_Option ("self-test", ' '));

   function Version return Boolean
   is (WL.Command_Line.Find_Option ("version", 'v'));

   function Core_Size return Natural
   is (WL.Command_Line.Find_Option ("core-size", ' ', 1024));

   function Build return String
   is (WL.Command_Line.Find_Option ("build", 'b'));

   function Evaluate return String
   is (WL.Command_Line.Find_Option ("evaluate", 'e'));

   function Main return String
   is (WL.Command_Line.Find_Option ("main", 'm'));

   function Precompile return String
   is (WL.Command_Line.Find_Option ("precompile", ' '));

   --------------------------
   -- Iterate_Include_Paths --
   --------------------------

   procedure Iterate_Include_Paths
     (Process : not null access procedure (Path : String))
   is
      use Ada.Command_Line;

      Long   : constant String := "--include=";
      Index  : Natural := 1;
   begin
      while Index <= Argument_Count loop
         declare
            Arg : constant String := Argument (Index);
         begin
            if Arg = "-i" or else Arg = "--include" then
               if Index < Argument_Count then
                  Index := Index + 1;
                  Process (Argument (Index));
               end if;
            elsif Arg'Length > 2
              and then Arg (Arg'First .. Arg'First + 1) = "-i"
            then
               Process (Arg (Arg'First + 2 .. Arg'Last));
            elsif Arg'Length > Long'Length
              and then Arg (Arg'First .. Arg'First + Long'Length - 1) = Long
            then
               Process (Arg (Arg'First + Long'Length .. Arg'Last));
            end if;
         end;
         Index := Index + 1;
      end loop;
   end Iterate_Include_Paths;

end Leander.Command_Line;
