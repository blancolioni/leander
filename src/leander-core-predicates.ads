with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Predicates is

   type Instance is
     new Leander.Core.Abstraction
     and Leander.Showable.Abstraction
   with private;

   type Predicate_Array is array (Positive range <>) of Instance;

   function Class_Name
     (This : Instance'Class)
      return String;

   function Get_Type
     (This : Instance'Class)
      return Leander.Core.Types.Reference;

   function Predicate
     (Class_Name : String;
      For_Type   : Leander.Core.Types.Reference)
      return Instance;

private

   type Nullable_Type_Reference is
     access constant Leander.Core.Types.Instance'Class;

   type Instance is
     new Leander.Core.Abstraction
     and Leander.Showable.Abstraction with
      record
         Class_Name : Conid;
         For_Type   : Nullable_Type_Reference;
      end record;

   overriding function Location
     (This : Instance)
      return Leander.Source.Source_Location
   is (Leander.Source.No_Location);

   overriding function Show (This : Instance) return String
   is (Core.To_String (This.Class_Name)
       & " "
       & This.For_Type.Show);

   function Predicate
     (Class_Name : String;
      For_Type   : Leander.Core.Types.Reference)
      return Instance
   is (Core.To_Conid (Class_Name), Nullable_Type_Reference (For_Type));

   function Class_Name
     (This : Instance'Class)
      return String
   is (Core.To_String (This.Class_Name));

   function Get_Type
     (This : Instance'Class)
      return Leander.Core.Types.Reference
   is (Leander.Core.Types.Reference (This.For_Type));

end Leander.Core.Predicates;
