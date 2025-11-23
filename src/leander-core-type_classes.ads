with Leander.Core.Binding_Groups;
with Leander.Core.Type_Instances;
with Leander.Core.Predicates;
with Leander.Core.Schemes;
with Leander.Source;

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

   function Super_Classes
     (This : Instance'Class)
      return Leander.Core.Conid_Array;

   type Class_Environment is interface;

   function Get_Class
     (This : Class_Environment;
      Id   : Conid)
      return Reference
      is abstract;

   function Get_Instances
     (This : Class_Environment;
      Id   : Conid)
      return Leander.Core.Type_Instances.Reference_Array
      is abstract;

   function Super_Classes
     (This : Class_Environment'Class;
      Id   : Conid)
      return Leander.Core.Conid_Array;

   function All_Instances
     (This      : Class_Environment'Class;
      Class_Id  : Conid)
      return Leander.Core.Type_Instances.Reference_Array;

   function By_Instance
     (This      : Class_Environment'Class;
      Predicate : Leander.Core.Predicates.Instance;
      Success   : out Boolean)
      return Leander.Core.Predicates.Predicate_Array;

   function Entails
     (This    : Class_Environment'Class;
      Current : Core.Predicates.Predicate_Array;
      Check   : Core.Predicates.Instance)
      return Boolean;

   function To_Head_Normal_Form
     (This      : Class_Environment'Class;
      Predicate : Leander.Core.Predicates.Instance)
      return Leander.Core.Predicates.Predicate_Array;

   function Reduce
     (This       : Class_Environment'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Success    : out Boolean)
      return Leander.Core.Predicates.Predicate_Array;

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
