with Ada.Containers.Vectors;

package body Leander.Core.Type_Synonyms is

   type Nullable_Type_Reference is
     access constant Leander.Core.Types.Instance'Class;

   type Synonym_Record is
      record
         Name       : Conid;
         Arity      : Natural;
         Definition : Nullable_Type_Reference;
      end record;

   package Synonym_Vectors is
     new Ada.Containers.Vectors (Positive, Synonym_Record);

   Synonyms : Synonym_Vectors.Vector;

   function Index_Of (Name : Conid) return Natural;

   ---------
   -- Add --
   ---------

   procedure Add
     (Name       : Conid;
      Parameters : Varid_Array;
      Definition : Leander.Core.Types.Reference)
   is
      function Generalize
        (T : Leander.Core.Types.Reference)
         return Leander.Core.Types.Reference;

      ----------------
      -- Generalize --
      ----------------

      function Generalize
        (T : Leander.Core.Types.Reference)
         return Leander.Core.Types.Reference
      is
      begin
         if T.Is_Application then
            return Leander.Core.Types.Application
              (Generalize (T.Left), Generalize (T.Right));
         elsif T.Is_Variable then
            for I in Parameters'Range loop
               if Parameters (I) = T.Variable.Name then
                  return Leander.Core.Types.TGen
                    (I - Parameters'First + 1);
               end if;
            end loop;
            return T;
         else
            return T;
         end if;
      end Generalize;

   begin
      Add_Expanded (Name, Parameters'Length, Generalize (Definition));
   end Add;

   ------------------
   -- Add_Expanded --
   ------------------

   procedure Add_Expanded
     (Name       : Conid;
      Arity      : Natural;
      Definition : Leander.Core.Types.Reference)
   is
      Index : constant Natural := Index_Of (Name);
      Item  : constant Synonym_Record :=
                (Name       => Name,
                 Arity      => Arity,
                 Definition => Nullable_Type_Reference (Definition));
   begin
      --  A redeclaration replaces the earlier one rather than shadowing it,
      --  so that reloading a module cannot leave two entries under one name.
      if Index = 0 then
         Synonyms.Append (Item);
      else
         Synonyms (Index) := Item;
      end if;
   end Add_Expanded;

   -----------
   -- Arity --
   -----------

   function Arity (Name : Conid) return Natural
   is (Synonyms (Index_Of (Name)).Arity);

   -----------
   -- Clear --
   -----------

   procedure Clear is
   begin
      Synonyms.Clear;
   end Clear;

   -----------
   -- Count --
   -----------

   function Count return Natural
   is (Natural (Synonyms.Length));

   ------------
   -- Exists --
   ------------

   function Exists (Name : Conid) return Boolean
   is (Index_Of (Name) /= 0);

   ------------
   -- Expand --
   ------------

   function Expand
     (T : Leander.Core.Types.Reference)
      return Leander.Core.Types.Reference
   is
      Max_Args : constant := 32;

      Args  : Leander.Core.Types.Type_Array (1 .. Max_Args) :=
                [others => T];
      Count : Natural := 0;
      Walk  : Leander.Core.Types.Reference := T;
   begin
      --  Unwind the spine, collecting arguments outermost-last so that
      --  Args (I) is the synonym's I'th parameter.
      while Walk.Is_Application loop
         if Count = Max_Args then
            return T;
         end if;
         Count := Count + 1;
         Args (Count) := Walk.Right;
         Walk := Walk.Left;
      end loop;

      for I in 1 .. Count / 2 loop
         declare
            Swap : constant Leander.Core.Types.Reference := Args (I);
         begin
            Args (I) := Args (Count - I + 1);
            Args (Count - I + 1) := Swap;
         end;
      end loop;

      if not Walk.Is_Constructor then
         return T;
      end if;

      declare
         Head  : constant Conid := Walk.Constructor.Id;
         Index : constant Natural := Index_Of (Head);
      begin
         if Index = 0 then
            return T;
         end if;

         declare
            Item : Synonym_Record renames Synonyms (Index);
         begin
            if Count < Item.Arity then
               return T;
            end if;

            declare
               Result : Leander.Core.Types.Reference :=
                          Leander.Core.Types.Reference
                            (Item.Definition).Instantiate
                              (Args (1 .. Item.Arity));
            begin
               --  A nullary synonym may still be applied to arguments, as
               --  in "type L = []" used as "L Int"; anything past the
               --  parameters is re-applied to whatever it expanded to.
               for I in Item.Arity + 1 .. Count loop
                  Result :=
                    Leander.Core.Types.Application (Result, Args (I));
               end loop;
               return Result;
            end;
         end;
      end;
   end Expand;

   --------------
   -- Index_Of --
   --------------

   function Index_Of (Name : Conid) return Natural is
      use type Leander.Names.Leander_Name;
   begin
      for I in 1 .. Natural (Synonyms.Length) loop
         if Leander.Names.Leander_Name (Synonyms (I).Name)
           = Leander.Names.Leander_Name (Name)
         then
            return I;
         end if;
      end loop;
      return 0;
   end Index_Of;

   -------------------
   -- Synonym_Arity --
   -------------------

   function Synonym_Arity (Index : Positive) return Natural
   is (Synonyms (Index).Arity);

   ------------------------
   -- Synonym_Definition --
   ------------------------

   function Synonym_Definition
     (Index : Positive)
      return Leander.Core.Types.Reference
   is (Leander.Core.Types.Reference (Synonyms (Index).Definition));

   ------------------
   -- Synonym_Name --
   ------------------

   function Synonym_Name (Index : Positive) return Conid
   is (Synonyms (Index).Name);

end Leander.Core.Type_Synonyms;
