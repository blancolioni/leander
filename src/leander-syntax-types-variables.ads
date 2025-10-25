with Leander.Core.Tyvars;

private package Leander.Syntax.Types.Variables is

   subtype Parent is Leander.Syntax.Types.Instance;
   type Instance is new Parent with private;

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Types.Reference;

private

   type Instance is new Parent with
      record
         Name : Core.Varid;
      end record;

   overriding function Head
     (This : not null access constant Instance)
      return Reference
   is (Reference (This));

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Types.Reference
   is (Leander.Core.Types.TVar
       (Leander.Core.Tyvars.Tyvar
          (This.Name, This.Kind)));

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference
   is (new Instance'
         (Location => Location,
          Name     => Leander.Core.To_Varid (Name),
          others   => <>));

end Leander.Syntax.Types.Variables;
