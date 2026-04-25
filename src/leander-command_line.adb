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
   is (WL.Command_Line.Find_Option ("core-size", ' ', 256));

   function Build return String
   is (WL.Command_Line.Find_Option ("build", 'b'));

   function Evaluate return String
   is (WL.Command_Line.Find_Option ("evaluate", 'e'));

   function Main return String
   is (WL.Command_Line.Find_Option ("main", 'm'));

end Leander.Command_Line;
