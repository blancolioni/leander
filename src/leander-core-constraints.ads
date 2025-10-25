with Leander.Core.Types;
with Leander.Core.Tyvars;
with Leander.Showable;

package Leander.Core.Constraints is

   type Instance is
     new Leander.Core.Tyvars.Container_Abstraction
     and Leander.Showable.Abstraction
   with private;

private

   type Instance is
     new Leander.Core.Tyvars.Container_Abstraction
     and Leander.Showable.Abstraction with
      record
         Ty         : Leander.Core.Types.Reference;
         Constraint : Leander.Core.Types.Reference;
      end record;

   overriding function Show (This : Instance) return String;

end Leander.Core.Constraints;
