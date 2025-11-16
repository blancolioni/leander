with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Predicates is

   type Instance is
     new Leander.Core.Abstraction
     and Leander.Showable.Abstraction
   with private;

   type Predicate_Array is array (Positive range <>) of Instance;

   function Class_Id
     (This : Instance'Class)
      return Conid;

   function Class_Name
     (This : Instance'Class)
      return String;

   function Get_Type
     (This : Instance'Class)
      return Leander.Core.Types.Reference;

   function Predicate
     (Class_Id : Conid;
      For_Type : Leander.Core.Types.Reference)
      return Instance;

   function Predicate
     (Class_Name : String;
      For_Type   : Leander.Core.Types.Reference)
      return Instance;

   function Show (Ps : Predicate_Array) return String;

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

   overriding function Show (This : Instance) return String
   is (Core.To_String (This.Class_Name)
       & " "
       & This.For_Type.Show);

   function Predicate
     (Class_Id : Conid;
      For_Type : Leander.Core.Types.Reference)
      return Instance
   is (Class_Id, Nullable_Type_Reference (For_Type));

   function Predicate
     (Class_Name : String;
      For_Type   : Leander.Core.Types.Reference)
      return Instance
   is (Predicate (Core.To_Conid (Class_Name), For_Type));

   function Class_Id
     (This : Instance'Class)
      return Conid
   is (This.Class_Name);

   function Class_Name
     (This : Instance'Class)
      return String
   is (Core.To_String (This.Class_Name));

   function Get_Type
     (This : Instance'Class)
      return Leander.Core.Types.Reference
   is (Leander.Core.Types.Reference (This.For_Type));

end Leander.Core.Predicates;
