private with Ada.Containers.Vectors;

package Leander.Data_Types.Builder is

   type Data_Type_Builder is tagged private;

   procedure Start
     (This  : in out Data_Type_Builder'Class;
      Tycon : Leander.Core.Types.Reference;
      Kind  : Leander.Core.Kinds.Kind);

   procedure Add_Con
     (This   : in out Data_Type_Builder'Class;
      Name   : Leander.Core.Conid;
      Scheme : Leander.Core.Schemes.Reference);

   procedure Build
     (This   : in out Data_Type_Builder'Class);

   function Data_Type
     (This : Data_Type_Builder'Class)
      return Reference;

private

   type Nullable_Scheme_Reference is
     access constant Leander.Core.Schemes.Instance'Class;

   type Con_Scheme is
      record
         Id     : Core.Conid;
         Scheme : Nullable_Scheme_Reference;
      end record;

   package Con_Record_Vectors is
     new Ada.Containers.Vectors (Positive, Con_Scheme);

   type Nullable_Type_Reference is
     access constant Leander.Core.Types.Instance'Class;

   type Nullable_Data_Type is access constant Instance'Class;

   type Data_Type_Builder is tagged
      record
         Id    : Core.Conid;
         Tycon : Nullable_Type_Reference;
         Kind  : Leander.Core.Kinds.Kind;
         Cons  : Con_Record_Vectors.Vector;
         DT    : Nullable_Data_Type;
      end record;

end Leander.Data_Types.Builder;
