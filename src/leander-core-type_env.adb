with Ada.Strings.Unbounded;
with Leander.Allocator;
with Leander.Logging;

package body Leander.Core.Type_Env is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("type_environments", Instance, Variable_Reference);

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Allocator.Allocate (Instance (This))));

   Local_Empty_Env : aliased constant Instance :=
                       Instance'(Map => <>, Next => null);

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance
   is
      Result : Instance;
      It     : Reference := Reference (This);
   begin
      while It /= null loop
         declare
            procedure Copy
              (Key     : Leander.Names.Leander_Name;
               Element : Nullable_Scheme_Reference);

            ----------
            -- Copy --
            ----------

            procedure Copy
              (Key     : Leander.Names.Leander_Name;
               Element : Nullable_Scheme_Reference)
            is
            begin
               if not Result.Map.Contains (Key) then
                  Result.Map.Insert (Key, Element.Apply (Subst));
               end if;
            end Copy;

         begin
            It.Map.Iterate (Copy'Access);
            It := It.Next;
         end;
      end loop;
      return Allocate (Result);
   end Apply;

   -------------
   -- Compose --
   -------------

   function Compose
     (This   : not null access constant Instance'Class;
      Name   : Varid;
      Scheme : Leander.Core.Schemes.Reference)
      return access constant Instance
   is
   begin
      return Allocate
        (Instance'
           (Map  => Scheme_Maps.Singleton
                (Leander.Names.Leander_Name (Name),
                 Nullable_Scheme_Reference (Scheme)),
            Next => Reference (This)));
   end Compose;

   -------------
   -- Compose --
   -------------

   function Compose
     (This   : not null access constant Instance'Class;
      That   : not null access constant Instance'Class)
      return access constant Instance
   is
      Result : Instance :=
                 Instance'
                   (Map => This.Map,
                    Next => Reference (That));
      It     : Reference := This.Next;

      procedure Save (Key     : Leander.Names.Leander_Name;
                      Element : Nullable_Scheme_Reference);

      ----------
      -- Save --
      ----------

      procedure Save (Key     : Leander.Names.Leander_Name;
                      Element : Nullable_Scheme_Reference)
      is
      begin
         Result.Map.Insert (Key, Element);
      end Save;

   begin
      while It /= null loop
         It.Map.Iterate (Save'Access);
         It := It.Next;
      end loop;
      return Allocate (Result);
   end Compose;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean
   is
   begin
      return This.Contains
        (Leander.Names.Leander_Name (Tyvar.Name));
   end Contains;

   --------------
   -- Contains --
   --------------

   function Contains
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Boolean
   is
   begin
      return This.Map.Contains (Name)
        or else (This.Next /= null
                 and then This.Next.Contains (Name));
   end Contains;

   -------------
   -- Dispose --
   -------------

   overriding procedure Dispose (This : in out Instance) is
   begin
      null;
   end Dispose;

   -------------
   -- Element --
   -------------

   function Element
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Schemes.Reference
   is
   begin
      if This.Map.Contains (Name) then
         return Leander.Core.Schemes.Reference
           (This.Map.Element (Name));
      else
         return This.Next.Element (Name);
      end if;
   end Element;

   -----------
   -- Empty --
   -----------

   function Empty return Reference is
   begin
      return Local_Empty_Env'Access;
   end Empty;

   ------------------
   -- Get_Type_Env --
   ------------------

   function Get_Type_Env
     (This : Builder)
      return Reference
   is
   begin
      return Allocate (This.Type_Env);
   end Get_Type_Env;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array
   is
      Builder : Tyvars.Tyvar_Array_Builder;

      procedure Include
        (Key    : Leander.Names.Leander_Name;
         Scheme : Nullable_Scheme_Reference);

      -------------
      -- Include --
      -------------

      procedure Include
        (Key    : Leander.Names.Leander_Name;
         Scheme : Nullable_Scheme_Reference)
      is
         pragma Unreferenced (Key);
      begin
         Builder.Include (Scheme.Get_Tyvars);
      end Include;

   begin
      This.Map.Iterate (Include'Access);
      return Builder.To_Tyvar_Array;
   end Get_Tyvars;

   ---------
   -- Ids --
   ---------

   function Ids (This : Instance'Class) return Leander.Names.Name_Array is
      use type Leander.Names.Name_Array;
   begin
      if This.Next = null then
         return This.Map.Get_Keys;
      else
         return This.Map.Get_Keys & This.Next.Ids;
      end if;
   end Ids;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Nullable_Scheme_Reference
   is
   begin
      if This.Map.Contains (Name) then
         return This.Map.Element (Name);
      elsif This.Next /= null then
         return This.Next.Lookup (Name);
      else
         return null;
      end if;
   end Lookup;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

   ----------
   -- Save --
   ----------

   function Save
     (This    : not null access constant Instance'Class;
      To      : not null access constant Instance'Class;
      Subst   : Leander.Core.Substitutions.Instance'Class)
      return Reference
   is
      Result : Instance :=
                 Instance'
                   (Map => This.Map,
                    Next => Reference (To));
      It     : Reference := This.Next;

      procedure Save (Key     : Leander.Names.Leander_Name;
                      Element : Nullable_Scheme_Reference);

      ----------
      -- Save --
      ----------

      procedure Save (Key     : Leander.Names.Leander_Name;
                      Element : Nullable_Scheme_Reference)
      is
         Sc : constant Leander.Core.Schemes.Reference :=
               Element.Apply (Subst);
      begin
         if not Result.Map.Contains (Key) then
            Result.Map.Insert (Key, Nullable_Scheme_Reference (Sc));
            Leander.Logging.Log
              ("ENV",
               Leander.Names.To_String (Key)
               & " :: "
               & Sc.Show);
         end if;
      end Save;

   begin
      Leander.Logging.Log
        ("ENV", Subst.Show);
      while It /= null loop
         It.Map.Iterate (Save'Access);
         It := It.Next;
      end loop;
      return Allocate (Result);
   end Save;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
      use Ada.Strings.Unbounded;
      Image : Unbounded_String;
      It    : Reference := This'Unchecked_Access;
   begin
      while It /= null loop
         declare
            procedure Append (Key     : Leander.Names.Leander_Name;
                              Element : Nullable_Scheme_Reference);

            ------------
            -- Append --
            ------------

            procedure Append (Key     : Leander.Names.Leander_Name;
                              Element : Nullable_Scheme_Reference)
            is
            begin
               if Image /= "" then
                  Image := @ & ";";
               end if;
               Image := Image & Leander.Names.To_String (Key)
                 & "=" & Element.Show;
            end Append;

         begin
            It.Map.Iterate (Append'Access);
            It := It.Next;
         end;
      end loop;
      return "{" & To_String (Image) & "}";
   end Show;

end Leander.Core.Type_Env;
