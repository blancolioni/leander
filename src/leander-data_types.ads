with Leander.Calculus;
with Leander.Core.Kinds;
with Leander.Core.Schemes;
with Leander.Core.Types;

package Leander.Data_Types is

   type Instance (<>) is tagged private;
   type Reference is access constant Instance'Class;

   function Id (This : Instance'Class) return Core.Conid;

   function Constructor_Count
     (This : Instance'Class)
      return Natural;

   function Constructor_Name
     (This  : Instance'Class;
      Index : Positive)
      return Core.Conid;

   function Constructor_Type
     (This  : Instance'Class;
      Index : Positive)
      return Leander.Core.Schemes.Reference;

   function Constructor_Calculus
     (This  : Instance'Class;
      Index : Positive)
      return Leander.Calculus.Tree;

   function Constructor_Index
     (This : Instance'Class;
      Id   : Core.Conid)
      return Natural;

private

   type Con_Record is
      record
         Con_Name : Core.Conid;
         Con_Type : Leander.Core.Schemes.Reference;
         Con_Defn : Leander.Calculus.Tree;
      end record;

   type Con_Array is array (Positive range <>) of Con_Record;

   type Instance (Con_Count : Positive) is tagged
      record
         Id    : Core.Conid;
         Tycon : Leander.Core.Types.Reference;
         Kind  : Leander.Core.Kinds.Kind;
         Cons  : Con_Array (1 .. Con_Count);
      end record;

   function Id (This : Instance'Class) return Core.Conid
   is (This.Id);

   function Constructor_Count
     (This : Instance'Class)
      return Natural
   is (This.Con_Count);

   function Constructor_Name
     (This  : Instance'Class;
      Index : Positive)
      return Core.Conid
   is (This.Cons (Index).Con_Name);

   function Constructor_Type
     (This  : Instance'Class;
      Index : Positive)
      return Leander.Core.Schemes.Reference
   is (This.Cons (Index).Con_Type);

   function Constructor_Calculus
     (This  : Instance'Class;
      Index : Positive)
      return Leander.Calculus.Tree
   is (This.Cons (Index).Con_Defn);

end Leander.Data_Types;
