with Ada.Containers.Doubly_Linked_Lists;
with Leander.Core.Types;

package body Leander.Core.Substitutions is

   type Entry_Record is
      record
         Tyvar   : Leander.Core.Tyvars.Reference;
         Binding : Type_Reference;
      end record;

   package List_Of_Entries is
     new Ada.Containers.Doubly_Linked_Lists (Entry_Record);

   type Instance is new Abstraction with
      record
         List : List_Of_Entries.List;
      end record;

   overriding function Show
     (This : Instance)
      return String;

   overriding function Lookup
     (This  : Instance;
      Tyvar : Core.Tyvars.Reference)
      return Maybe_Result.Maybe;

   overriding function Merge
     (This  : not null access constant Instance;
      Other : not null access constant Abstraction'Class)
      return Reference;

   Local_Empty_Substitution : aliased constant Instance :=
                                (List => []);

   function Empty return Reference
   is (Local_Empty_Substitution'Access);

   ------------
   -- Lookup --
   ------------

   overriding function Lookup
     (This  : Instance;
      Tyvar : Core.Tyvars.Reference)
      return Maybe_Result.Maybe
   is
   begin
      for Element of This.List loop
         if Element.Tyvar.Name = Tyvar.Name then
            return Maybe_Result.Just (Element.Binding);
         end if;
      end loop;
      return Maybe_Result.Nothing;
   end Lookup;

   -----------
   -- Merge --
   -----------

   overriding function Merge
     (This  : not null access constant Instance;
      Other : not null access constant Abstraction'Class)
      return Reference
   is
      Result : Instance;
   begin
      for Rec of Instance (Other.all).List loop
         Result.List.Append
           (Entry_Record'
              (Tyvar => Rec.Tyvar,
               Binding => Type_Reference (Rec.Binding.Apply (This))));
      end loop;
      for Rec of This.List loop
         Result.List.Append (Rec);
      end loop;
      return new Instance'(Result);
   end Merge;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
      function Show_List
        (Position : List_Of_Entries.Cursor)
         return String;

      ---------------
      -- Show_List --
      ---------------

      function Show_List
        (Position : List_Of_Entries.Cursor)
         return String
      is
      begin
         if not List_Of_Entries.Has_Element (Position) then
            return "";
         else
            declare
               Item : constant Entry_Record :=
                        List_Of_Entries.Element (Position);
               Sep  : constant String :=
                        (if List_Of_Entries."=" (Position, This.List.First)
                         then ""
                         else ";");
            begin
               return Sep
                 & Item.Tyvar.Show
                 & "="
                 & Item.Binding.Show
                 & Show_List (List_Of_Entries.Next (Position));
            end;
         end if;
      end Show_List;

   begin
      return "[" & Show_List (This.List.First) & "]";
   end Show;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Tyvar      : Leander.Core.Tyvars.Reference;
      Element    : not null access constant Types.Abstraction'Class)
      return Reference
   is
   begin
      return new Instance'(List => [(Tyvar, Type_Reference (Element))]);
   end Substitute;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Tyvars   : Leander.Core.Tyvars.Tyvar_Array;
      Elements : Type_Reference_Array)
      return Reference
   is
      This : Instance;
      I : Natural := Elements'First - 1;
   begin
      for Tv of Tyvars loop
         I := I + 1;
         This.List.Append (Entry_Record'(Tv, Elements (I)));
      end loop;
      return new Instance'(This);
   end Substitute;

end Leander.Core.Substitutions;
