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

   ---------------
   -- Has_Label --
   ---------------

   function Has_Label
     (This  : Instance'Class;
      Label : String)
      return Boolean
   is (for some I in 1 .. This.Con_Count =>
         This.Label_Position (I, Label) > 0);

   ------------
   -- Labels --
   ------------

   function Labels
     (This : Instance'Class)
      return Leander.Names.Name_Array
   is
      use type Leander.Names.Name_Array;

      function From (Index : Positive) return Leander.Names.Name_Array;

      ----------
      -- From --
      ----------

      function From (Index : Positive) return Leander.Names.Name_Array is
      begin
         if Index > This.Con_Count then
            return [];
         end if;

         declare
            Own   : constant Leander.Names.Name_Array :=
                      This.Constructor_Labels (Index);
            Fresh : Leander.Names.Name_Array (1 .. Own'Length);
            Count : Natural := 0;
         begin
            for Label of Own loop
               if not (for some Earlier in 1 .. Index - 1 =>
                         This.Label_Position
                           (Earlier, Leander.Names.To_String (Label)) > 0)
               then
                  Count := Count + 1;
                  Fresh (Count) := Label;
               end if;
            end loop;
            return Fresh (1 .. Count) & From (Index + 1);
         end;
      end From;

   begin
      return From (1);
   end Labels;

   --------------------
   -- Label_Position --
   --------------------

   function Label_Position
     (This  : Instance'Class;
      Index : Positive;
      Label : String)
      return Natural
   is
      Labels : constant Leander.Names.Name_Array :=
                 This.Constructor_Labels (Index);
   begin
      for I in Labels'Range loop
         if Leander.Names.To_String (Labels (I)) = Label then
            return I - Labels'First + 1;
         end if;
      end loop;
      return 0;
   end Label_Position;

end Leander.Data_Types;
