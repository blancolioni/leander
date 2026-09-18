package body Leander.Data_Types is

   -----------------------
   -- Constructor_Arity --
   -----------------------

   function Constructor_Arity
     (This  : Instance'Class;
      Index : Positive)
      return Natural
   is (Scheme_Arity (This.Cons (Index).Con_Type));

   ------------------
   -- Scheme_Arity --
   ------------------

   function Scheme_Arity
     (Scheme : Leander.Core.Schemes.Reference)
      return Natural
   is
      use type Leander.Core.Types.Reference;
      T     : Leander.Core.Types.Reference := Scheme.Inner_Type;
      Count : Natural := 0;
   begin
      while T.Is_Application
        and then T.Left.Is_Application
        and then T.Left.Left = Leander.Core.Types.T_Arrow
      loop
         Count := Count + 1;
         T := T.Right;
      end loop;
      return Count;
   end Scheme_Arity;

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
