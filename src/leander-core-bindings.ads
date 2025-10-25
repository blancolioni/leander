with Leander.Core.Alts;
with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Bindings is

   type Instance (<>) is
     new Leander.Showable.Abstraction
   with private;

   type Reference is access constant Instance'Class;

   type Reference_Array is array (Positive range <>) of Reference;

   function Name (This : Instance) return Varid;

   function Alts
     (This : Instance)
      return Leander.Core.Alts.Reference_Array;

   function Implicit_Binding
     (Name : Varid;
      Alts : Leander.Core.Alts.Reference_Array)
      return Reference;

   procedure Prune;

private

   type Nullable_Type_Reference is
     access all Leander.Core.Types.Instance'Class;

   type Instance (Alt_Count : Positive) is
     new Leander.Showable.Abstraction with
      record
         Name          : Varid;
         Alts          : Leander.Core.Alts.Reference_Array (1 .. Alt_Count);
         Explicit_Type : Nullable_Type_Reference;
      end record;

   overriding function Show (This : Instance) return String;

   function Name (This : Instance) return Varid
   is (This.Name);

   function Alts
     (This : Instance)
      return Leander.Core.Alts.Reference_Array
   is (This.Alts);

end Leander.Core.Bindings;
