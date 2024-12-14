package body Leander.Core.Tycons is

   type Instance is new Abstraction with
      record
         Id    : Name_Id;
         Kind  : Kinds.Reference;
      end record;

   overriding function Kind (This : Instance) return Kinds.Reference
   is (This.Kind);

   overriding function Show
     (This : Instance)
      return String
   is (Show (This.Id));

   overriding function Name (This : Instance) return Name_Id
   is (This.Id);

   -----------
   -- Tycon --
   -----------

   function Tycon
     (Id   : Name_Id;
      Kind : Kinds.Reference)
      return Reference
   is
      This : constant Instance := Instance'(Id, Kind);
   begin
      return new Instance'(This);
   end Tycon;

end Leander.Core.Tycons;
