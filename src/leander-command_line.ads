package Leander.Command_Line is

   function Enable_Log return Boolean;
   function Report return Boolean;
   function Self_Test return Boolean;
   function Version return Boolean;

   function Core_Size return Natural;

   function Build return String;
   function Evaluate return String;
   function Main return String;
   function Precompile return String;

   procedure Iterate_Include_Paths
     (Process : not null access procedure (Path : String));
   --  Call Process once per "-i DIR" / "--include=DIR" on the command
   --  line, in the order they were written. Unlike the options above this
   --  one may be repeated, so it reads Ada.Command_Line directly rather
   --  than going through WL.Command_Line, which only ever reports an
   --  option's first occurrence.

end Leander.Command_Line;
