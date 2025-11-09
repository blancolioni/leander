with Leander.Core.Tycons;
package body Leander.Syntax.Types.Constructors is

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Types.Reference
   is
      Id : constant String := Core.To_String (This.Name);
   begin
      if Id = "(->)" then
         return Core.Types.T_Arrow;
      elsif Id = "[]" then
         return Core.Types.T_List;
      else
         return Leander.Core.Types.TCon
           (Leander.Core.Tycons.Tycon
              (This.Name, This.Kind));
      end if;
   end To_Core;

end Leander.Syntax.Types.Constructors;
