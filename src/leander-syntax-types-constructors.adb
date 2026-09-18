with Leander.Core.Tycons;
with Leander.Core.Type_Synonyms;

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
         --  Expand covers a synonym of no parameters, which appears here
         --  as a bare constructor; one with parameters is a spine, and is
         --  expanded by the application node at the top of it.
         return Leander.Core.Type_Synonyms.Expand
           (Leander.Core.Types.TCon
              (Leander.Core.Tycons.Tycon
                 (This.Name, This.Kind)));
      end if;
   end To_Core;

end Leander.Syntax.Types.Constructors;
