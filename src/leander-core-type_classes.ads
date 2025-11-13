with Leander.Core.Binding_Groups;
with Leander.Core.Predicates;
with Leander.Core.Schemes;

package Leander.Core.Type_Classes is

   type Instance (<>) is tagged private;
   type Reference is access constant Instance'Class;

   function Id (This : Instance'Class) return Conid;

   function Type_Class
     (Class_Name    : Conid;
      Variable_Name : Varid;
      Predicates    : Leander.Core.Predicates.Predicate_Array;
      Bindings      : Leander.Core.Binding_Groups.Reference)
      return Reference;

   function Methods
     (This : Instance'Class)
      return Varid_Array;

   function Method_Location
     (This : Instance'Class;
      Id   : Varid)
      return Leander.Source.Source_Location;

   function Method_Scheme
     (This : Instance'Class;
      Id   : Varid)
      return Leander.Core.Schemes.Reference;

private

   type Instance (Predicate_Count : Natural) is tagged
      record
         Class_Id    : Conid;
         Var_Id      : Varid;
         Predicates  : Core.Predicates.Predicate_Array (1 .. Predicate_Count);
         Bindings    : Leander.Core.Binding_Groups.Reference;
      end record;

   function Id (This : Instance'Class) return Conid
   is (This.Class_Id);

   function Methods
     (This : Instance'Class)
      return Varid_Array
   is (This.Bindings.Varids);

   function Method_Location
     (This : Instance'Class;
      Id   : Varid)
      return Leander.Source.Source_Location
   is (Leander.Source.No_Location);

   function Method_Scheme
     (This : Instance'Class;
      Id   : Varid)
      return Leander.Core.Schemes.Reference
   is (This.Bindings.Lookup (Leander.Names.Leander_Name (Id)).Scheme);

end Leander.Core.Type_Classes;
