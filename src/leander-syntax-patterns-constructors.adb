package body Leander.Syntax.Patterns.Constructors is

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Patterns.Reference
   is
      Args : Core.Patterns.Conargs (1 .. This.Arg_Count);
   begin
      for I in This.Args'Range loop
         pragma Assert (This.Args (I).Is_Variable);
         Args (I) :=
           Core.To_Varid
             (This.Args (I).Variable_Name);
      end loop;
      return Core.Patterns.Constructor (Core.Conid (This.Name), Args);
   end To_Core;

end Leander.Syntax.Patterns.Constructors;
