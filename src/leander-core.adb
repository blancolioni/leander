with Ada.Containers.Indefinite_Vectors;
with WL.String_Maps;

package body Leander.Core is

   package Name_Vectors is
     new Ada.Containers.Indefinite_Vectors (Name_Id, String);

   package Name_Maps is
     new WL.String_Maps (Name_Id);

   Name_Vector : Name_Vectors.Vector;
   Name_Map    : Name_Maps.Map;

   ---------
   -- "=" --
   ---------

   --  function "=" (Left  : Name_Id;
   --                Right : String)
   --                return Boolean
   --  is
   --  begin
   --     return Name_Vector (Left) = Right;
   --  end "=";

   --------
   -- Id --
   --------

   function Id (S : String) return Name_Id is
   begin
      if not Name_Map.Contains (S) then
         Name_Vector.Append (S);
         Name_Map.Insert (S, Name_Vector.Last_Index);
      end if;
      return Name_Map (S);
   end Id;

   ----------
   -- Show --
   ----------

   function Show (Id : Name_Id) return String is
   begin
      return Name_Vector (Id);
   end Show;

end Leander.Core;
