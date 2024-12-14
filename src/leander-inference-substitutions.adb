with Ada.Containers.Doubly_Linked_Lists;
with Leander.Core.Tycons;

package body Leander.Inference.Substitutions is

   type Entry_Record is
      record
         Tyvar   : Leander.Core.Tyvars.Reference;
         Binding : Leander.Core.Types.Reference;
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
      return Maybe_Types.Maybe;

   overriding function Merge
     (This  : not null access constant Instance;
      Other : not null access constant Abstraction'Class)
      return Reference;

   overriding function Apply
     (This : not null access constant Instance;
      To   : Core.Types.Reference)
      return Core.Types.Reference;

   Local_Empty_Substitution : aliased constant Instance :=
                                (List => []);

   function Empty return Reference
   is (Local_Empty_Substitution'Access);

   type Subst_Application_Visitor is
     new Core.Types.Type_Visitor with
      record
         Subst  : Reference;
         Result : Leander.Core.Types.Reference := Leander.Core.Types.T_Error;
      end record;

   overriding procedure Variable
     (This  : in out Subst_Application_Visitor;
      Tyvar : Core.Tyvars.Reference);

   overriding procedure Constructor
     (This  : in out Subst_Application_Visitor;
      Tycon : Core.Tycons.Reference);

   overriding procedure Application
     (This        : in out Subst_Application_Visitor;
      Left, Right : Core.Types.Reference);

   -----------------
   -- Application --
   -----------------

   overriding procedure Application
     (This        : in out Subst_Application_Visitor;
      Left, Right : Core.Types.Reference)
   is
   begin
      This.Result :=
        This.Subst.Apply (Left)
        .Apply (This.Subst.Apply (Right));
   end Application;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This : not null access constant Instance;
      To   : Core.Types.Reference)
      return Core.Types.Reference
   is
   begin
      if To.Is_Application then
         return This.Apply (To.Left).Apply (This.Apply (To.Right));
      elsif To.Is_Constructor then
         return To;
      elsif To.Is_Variable then
         declare
            Maybe_Binding : constant Maybe_Types.Maybe :=
                              This.Lookup (To.Variable);
         begin
            if Maybe_Binding.Is_Nothing then
               return To;
            else
               return Maybe_Binding.From_Just;
            end if;
         end;
      else
         raise Program_Error with
           "cannot understand type: " & To.Show;
      end if;
   end Apply;

   -----------------
   -- Constructor --
   -----------------

   overriding procedure Constructor
     (This  : in out Subst_Application_Visitor;
      Tycon : Core.Tycons.Reference)
   is
   begin
      This.Result := Core.Types.TCon (Tycon);
   end Constructor;

   ------------
   -- Lookup --
   ------------

   overriding function Lookup
     (This  : Instance;
      Tyvar : Core.Tyvars.Reference)
      return Maybe_Types.Maybe
   is
      use type Leander.Core.Name_Id;
   begin
      for Element of This.List loop
         if Element.Tyvar.Name = Tyvar.Name then
            return Maybe_Types.Just (Element.Binding);
         end if;
      end loop;
      return Maybe_Types.Nothing;
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
               Binding => This.Apply (Rec.Binding)));
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
      Bound_Type : Leander.Core.Types.Reference)
      return Reference
   is
   begin
      return new Instance'(List => [(Tyvar, Bound_Type)]);
   end Substitute;

   --------------
   -- Variable --
   --------------

   overriding procedure Variable
     (This  : in out Subst_Application_Visitor;
      Tyvar : Core.Tyvars.Reference)
   is
      Maybe_Binding : constant Maybe_Types.Maybe :=
                        This.Subst.Lookup (Tyvar);
   begin
      if Maybe_Binding.Is_Nothing then
         This.Result := Core.Types.TVar (Tyvar);
      else
         This.Result := Maybe_Binding.From_Just;
      end if;
   end Variable;

end Leander.Inference.Substitutions;
