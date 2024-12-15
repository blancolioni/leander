with Ada.Containers.Doubly_Linked_Lists;

package body Leander.Core.Assumptions is

   type Entry_Record is
      record
         Id      : Name_Id;
         Binding : Leander.Core.Schemes.Reference;
      end record;

   package List_Of_Entries is
      new Ada.Containers.Doubly_Linked_Lists (Entry_Record);

   type Instance is new Abstraction with
      record
         List : List_Of_Entries.List;
      end record;

   overriding function Apply
     (This  : Instance;
      Subst : Substitutions.Reference)
      return Reference;

   overriding function Contains
     (This  : Instance;
      Tyvar : Tyvars.Reference)
      return Boolean;

   overriding function Get_Tyvars
     (This  : Instance)
      return Tyvars.Tyvar_Array;

   overriding function Find
     (This : Instance;
      Id   : Name_Id)
      return Maybe_Schemes.Maybe;

   overriding function Append
     (This    : Instance;
      Id      : Name_Id;
      Binding : Schemes.Reference)
      return Reference;

   overriding function Append
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference;

   overriding function Prepend
     (This    : Instance;
      Id      : Name_Id;
      Binding : Schemes.Reference)
      return Reference;

   overriding function Prepend
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference;

   Local_Empty_Assumptions : aliased constant Instance :=
                               (List => []);

   function Empty return Reference
   is (Local_Empty_Assumptions'Access);

   ------------
   -- Append --
   ------------

   overriding function Append
     (This    : Instance;
      Id      : Name_Id;
      Binding : Schemes.Reference)
      return Reference
   is
      Result : Instance := This;
   begin
      Result.List.Append (Entry_Record'(Id, Binding));
      return new Instance'(Result);
   end Append;

   ------------
   -- Append --
   ------------

   overriding function Append
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference
   is
      Result : Instance := This;
   begin
      for Element of Instance (That.all).List loop
         Result.List.Append (Element);
      end loop;
      return new Instance'(Result);
   end Append;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : Instance;
      Subst : Substitutions.Reference)
      return Reference
   is
      List : List_Of_Entries.List;
   begin
      for Element of This.List loop
         List.Append
           (Entry_Record'
              (Id => Element.Id,
               Binding => Element.Binding.Apply (Subst)));
      end loop;
      return new Instance'(List => List);
   end Apply;

   ----------------
   -- Assumption --
   ----------------

   function Assumption
     (Id     : Name_Id;
      Scheme : Schemes.Reference)
      return Reference
   is
   begin
      return Empty.Append (Id, Scheme);
   end Assumption;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (This  : Instance;
      Tyvar : Tyvars.Reference)
      return Boolean
   is
   begin
      for Tv of Instance'Class (This).Get_Tyvars loop
         if Tv.Name = Tyvar.Name then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   ----------
   -- Find --
   ----------

   overriding function Find
     (This : Instance;
      Id   : Name_Id)
      return Maybe_Schemes.Maybe
   is
   begin
      for Element of This.List loop
         if Element.Id = Id then
            return Maybe_Schemes.Just (Element.Binding);
         end if;
      end loop;
      return Maybe_Schemes.Nothing;
   end Find;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Tyvars.Tyvar_Array
   is
      function Find_Tyvars
        (Position : List_Of_Entries.Cursor)
         return Tyvars.Tyvar_Array;

      -----------------
      -- Find_Tyvars --
      -----------------

      function Find_Tyvars
        (Position : List_Of_Entries.Cursor)
         return Tyvars.Tyvar_Array
      is
      begin
         if not List_Of_Entries.Has_Element (Position) then
            return [];
         else
            declare
               use type Tyvars.Tyvar_Array;
               Rest : constant Tyvars.Tyvar_Array :=
                        Find_Tyvars (List_Of_Entries.Next (Position));
               Tvs  : constant Tyvars.Tyvar_Array :=
                        List_Of_Entries.Element (Position).Binding.Get_Tyvars;
            begin
               return Tvs & Rest;
            end;
         end if;
      end Find_Tyvars;

   begin
      return Tyvars.Nub (Find_Tyvars (This.List.First));
   end Get_Tyvars;

   -------------
   -- Prepend --
   -------------

   overriding function Prepend
     (This    : Instance;
      Id      : Name_Id;
      Binding : Schemes.Reference)
      return Reference
   is
      Result : Instance := This;
   begin
      Result.List.Prepend (Entry_Record'(Id, Binding));
      return new Instance'(Result);
   end Prepend;

   -------------
   -- Prepend --
   -------------

   overriding function Prepend
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference
   is
      Result : Instance := Instance (That.all);
   begin
      for Element of This.List loop
         Result.List.Append (Element);
      end loop;
      return new Instance'(Result);
   end Prepend;
end Leander.Core.Assumptions;
