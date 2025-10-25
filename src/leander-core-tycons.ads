with Leander.Core.Kinds;
with Leander.Names;
with Leander.Showable;

package Leander.Core.Tycons is

   type Instance is
     new Kinds.Has_Kind
     and Showable.Abstraction
   with private;

   function Id
     (This : Instance)
      return Conid;

   function Tycon
     (Id   : Conid;
      Kind : Leander.Core.Kinds.Kind)
      return Instance;

private

   type Instance is
     new Kinds.Has_Kind
     and Showable.Abstraction with
      record
         Id   : Conid;
         Kind : Leander.Core.Kinds.Kind;
      end record;

   overriding function Get_Kind
     (This : Instance)
      return Leander.Core.Kinds.Kind
   is (This.Kind);

   overriding function Show
     (This : Instance)
      return String
   is (To_String (This.Id));

   function Id
     (This : Instance)
      return Conid
   is (This.Id);

   function Tycon
     (Id   : Conid;
      Kind : Leander.Core.Kinds.Kind)
      return Instance
   is (Instance'(Id, Kind));

   function To_Conid (S : String) return Conid
   is (Conid (Leander.Names.To_Leander_Name (S)));

end Leander.Core.Tycons;
