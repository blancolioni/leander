package Leander.Command_Line is

   function Enable_Log return Boolean;
   function Report return Boolean;
   function Self_Test return Boolean;
   function Version return Boolean;

   function Core_Size return Natural;

   function Build return String;
   function Evaluate return String;
   function Main return String;

end Leander.Command_Line;
