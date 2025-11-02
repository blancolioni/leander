package body Leander.Data_Types is

   -----------------------
   -- Constructor_Index --
   -----------------------

   function Constructor_Index
     (This : Instance'Class;
      Id   : Core.Conid)
      return Natural
   is
      use type Leander.Core.Conid;
   begin
      for I in 1 .. This.Con_Count loop
         if This.Cons (I).Con_Name = Id then
            return I;
         end if;
      end loop;
      return 0;
   end Constructor_Index;

end Leander.Data_Types;
