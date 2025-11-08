with Leander.Core.Kinds;
with Leander.Core.Types;

package Leander.Syntax.Types is

   subtype Parent is Leander.Syntax.Instance;
   type Instance is abstract new Parent with private;
   type Reference is access constant Instance'Class;

   function Kind (This : Instance) return Leander.Core.Kinds.Kind;

   function To_Core (This : Instance) return Leander.Core.Types.Reference
                     is abstract;

   function Is_Variable (This : Instance) return Boolean;
   function Variable_Name (This : Instance) return String;

   function Head
     (This : not null access constant Instance)
      return Reference
      is abstract;

   function Variable
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   function Constructor
     (Location : Leander.Source.Source_Location;
      Name     : String)
      return Reference;

   function Application
     (Location    : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference;

private

   type Instance is abstract new Parent with
      record
         Kind : Leander.Core.Kinds.Kind :=
                  Leander.Core.Kinds.Star;
      end record;

   function Is_Variable (This : Instance) return Boolean is (False);
   function Variable_Name (This : Instance) return String is ("");

   function Kind (This : Instance) return Leander.Core.Kinds.Kind
   is (This.Kind);

end Leander.Syntax.Types;
