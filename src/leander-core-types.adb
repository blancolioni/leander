with Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;
with System.Storage_Pools;

with Leander.Names;

package body Leander.Core.Types is

   --  Two-region bump arena backing all dynamically allocated types
   --  (ADR 0001).  Allocation targets whichever region Mode selects; the
   --  scratch region is reset in one operation, the permanent region never.

   Block_Size : constant := 2 ** 20;
   Max_Blocks : constant := 4096;

   type Storage_Array_Access is
     access System.Storage_Elements.Storage_Array;

   type Memory_Array is array (1 .. Max_Blocks) of Storage_Array_Access;

   type Region is
      record
         Memory      : Memory_Array;
         Top         : System.Storage_Elements.Storage_Offset := 0;
         Next        : System.Storage_Elements.Storage_Offset := 0;
         Start       : System.Storage_Elements.Storage_Offset := 0;
         Block_Index : Natural := 0;
      end record;

   type Allocation_Mode is (Permanent, Scratch);

   type Type_Pool is new System.Storage_Pools.Root_Storage_Pool with
      record
         Perm : Region;
         Scr  : Region;
         Mode : Allocation_Mode := Permanent;
      end record;

   overriding procedure Allocate
     (Pool                     : in out Type_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count);

   overriding procedure Deallocate
     (Pool                     : in out Type_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is null;

   overriding function Storage_Size
     (Pool : Type_Pool)
      return System.Storage_Elements.Storage_Count
   is (System.Storage_Elements.Storage_Count'Last);

   Arena         : Type_Pool;
   Scratch_Depth : Natural := 0;

   --  Pool statistics (reported by Report).  Byte counts sum requested
   --  sizes; alignment padding and per-block slack are not included.
   Perm_Allocs    : Natural := 0;
   Scratch_Allocs : Natural := 0;
   Perm_Bytes     : System.Storage_Elements.Storage_Count := 0;
   Scratch_Cur    : System.Storage_Elements.Storage_Count := 0;
   Scratch_Peak   : System.Storage_Elements.Storage_Count := 0;

   type Variable_Reference is access all Instance'Class;
   for Variable_Reference'Storage_Pool use Arena;

   procedure Free is
     new Ada.Unchecked_Deallocation
       (System.Storage_Elements.Storage_Array, Storage_Array_Access);

   --------------
   -- Allocate --
   --------------

   overriding procedure Allocate
     (Pool                     : in out Type_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is
      use System.Storage_Elements;

      procedure Allocate_From (R : in out Region);

      -------------------
      -- Allocate_From --
      -------------------

      procedure Allocate_From (R : in out Region) is
      begin
         if R.Next mod Alignment /= 0 then
            R.Next := R.Next + Alignment - R.Next mod Alignment;
         end if;

         if R.Next + Size_In_Storage_Elements > R.Top then
            R.Next := R.Top;
            R.Start := R.Top;
            R.Block_Index := @ + 1;
            R.Memory (R.Block_Index) :=
              new Storage_Array (0 .. Block_Size - 1);
            R.Top := @ + Block_Size;
         end if;

         Storage_Address :=
           R.Memory (R.Block_Index) (R.Next - R.Start)'Address;
         R.Next := @ + Size_In_Storage_Elements;
      end Allocate_From;

   begin
      case Pool.Mode is
         when Permanent =>
            Allocate_From (Pool.Perm);
            Perm_Allocs := Perm_Allocs + 1;
            Perm_Bytes := Perm_Bytes + Size_In_Storage_Elements;
         when Scratch =>
            Allocate_From (Pool.Scr);
            Scratch_Allocs := Scratch_Allocs + 1;
            Scratch_Cur := Scratch_Cur + Size_In_Storage_Elements;
            if Scratch_Cur > Scratch_Peak then
               Scratch_Peak := Scratch_Cur;
            end if;
      end case;
   end Allocate;

   -------------------
   -- Begin_Scratch --
   -------------------

   procedure Begin_Scratch is
   begin
      Scratch_Depth := Scratch_Depth + 1;
      Arena.Mode := Scratch;
   end Begin_Scratch;

   -----------------
   -- End_Scratch --
   -----------------

   procedure End_Scratch is
   begin
      pragma Assert (Scratch_Depth > 0);
      Scratch_Depth := Scratch_Depth - 1;
      if Scratch_Depth = 0 then
         for Item of Arena.Scr.Memory (1 .. Arena.Scr.Block_Index) loop
            Free (Item);
         end loop;
         Arena.Scr := (others => <>);
         Arena.Mode := Permanent;
         Scratch_Cur := 0;
      end if;
   end End_Scratch;

   ------------
   -- Report --
   ------------

   procedure Report is
      use Ada.Text_IO;
   begin
      Put_Line ("type pool:");
      Put_Line ("  permanent size      :"
                & System.Storage_Elements.Storage_Count'Image (Perm_Bytes)
                & " bytes");
      Put_Line ("  max scratch size    :"
                & System.Storage_Elements.Storage_Count'Image (Scratch_Peak)
                & " bytes");
      Put_Line ("  total allocations   :"
                & Natural'Image (Perm_Allocs + Scratch_Allocs));
      Put_Line ("  permanent allocs    :" & Natural'Image (Perm_Allocs));
   end Report;

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean
   is (case This.Tag is
          when TVar => This.Tyvar.Name = Tyvar.Name,
          when TCon => False,
          when TGen => False,
          when TApp => This.Left.Contains (Tyvar)
       or else This.Right.Contains (Tyvar));

   function Allocate
     (This : Instance'Class)
      return Reference;

   function Con (Name : String;
                 Kind : Kinds.Kind := Kinds.Star)
                 return Instance
   is (TCon, Tycons.Tycon (To_Conid (Name), Kind));

   Kind_SS    : constant Kinds.Kind :=
                  Kinds.Kind_Function (Kinds.Star, Kinds.Star);
   Kind_SSS   : constant Kinds.Kind :=
                  Kinds.Kind_Function (Kinds.Star, Kind_SS);

   Local_TUnit    : aliased constant Instance := Con ("()");
   Local_TError   : aliased constant Instance := Con ("#error");
   Local_TChar    : aliased constant Instance := Con ("Char");
   Local_TInt     : aliased constant Instance := Con ("Int");
   Local_TInteger : aliased constant Instance := Con ("Integer");
   Local_TFloat   : aliased constant Instance := Con ("Float");
   Local_TDouble  : aliased constant Instance := Con ("Double");
   Local_TList    : aliased constant Instance := Con ("[]", Kind_SS);
   Local_TArrow   : aliased constant Instance := Con ("(->)", Kind_SSS);
   Local_TPair    : aliased constant Instance := Con ("(,)", Kind_SSS);

   Local_TString  : aliased constant Instance :=
                      (TApp, Local_TList'Access, Local_TChar'Access);

   function T_Arrow   return Reference is (Local_TArrow'Access);

   function T_Char   return Reference
   is (Local_TChar'Access);

   function T_Error   return Reference
   is (Local_TError'Access);

   function T_Int     return Reference
   is (Local_TInt'Access);

   function T_Integer return Reference
   is (Local_TInteger'Access);

   function T_Float   return Reference
   is (Local_TFloat'Access);

   function T_Double  return Reference
   is (Local_TDouble'Access);

   function T_List    return Reference
   is (Local_TList'Access);

   function T_Pair return Reference
   is (Local_TPair'Access);

   function T_Unit    return Reference
   is (Local_TUnit'Access);

   function T_String  return Reference is (Local_TString'Access);

   function Fn (From : Reference;
                To   : Reference)
                return Reference
   is (Application (Application (T_Arrow, From), To));

   function Pair (A, B : Reference) return Reference
   is (Application (Application (T_Pair, A), B));

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance'Class)
      return Reference
   is
   begin
      return Reference (Variable_Reference'(new Instance'Class'(This)));
   end Allocate;

   -----------------
   -- Application --
   -----------------

   function Application
     (Left, Right  : not null access constant Instance'Class)
      return Reference
   is
   begin
      return Allocate
        (Instance'(TApp, Reference (Left), Reference (Right)));
   end Application;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance
   is
   begin
      case This.Tag is
         when TVar =>
            declare
               use Leander.Core.Substitutions;
               T : constant Nullable_Type_Reference :=
                     Subst.Lookup
                       (Leander.Names.Leander_Name
                          (This.Tyvar.Name));
            begin
               if T = null then
                  return This;
               else
                  return T;
               end if;
            end;
         when TCon =>
            return This;
         when TApp =>
            return Application
              (Reference (This.Left.Apply (Subst)),
               Reference (This.Right.Apply (Subst)));
         when TGen =>
            return This;
      end case;
   end Apply;

   -------------
   -- Dispose --
   -------------

   overriding procedure Dispose (This : in out Instance) is
   begin
      null;
   end Dispose;

   ----------------
   -- Equivalent --
   ----------------

   function Equivalent
     (Left, Right : not null access constant Instance'Class)
      return Boolean
   is
   begin
      if Left.Tag /= Right.Tag then
         return False;
      end if;

      case Left.Tag is
         when TVar =>
            return Left.Tyvar.Name = Right.Tyvar.Name;
         when TCon =>
            return Left.Tycon.Id = Right.Tycon.Id;
         when TApp =>
            return Left.Left.Equivalent (Right.Left)
              and then Left.Right.Equivalent (Right.Right);
         when TGen =>
            return Left.Index = Right.Index;
      end case;
   end Equivalent;



   --------------
   -- Get_Kind --
   --------------

   overriding function Get_Kind
     (This : Instance)
      return Kinds.Kind
   is
   begin
      case This.Tag is
         when TVar =>
            return This.Tyvar.Get_Kind;
         when TCon =>
            return This.Tycon.Get_Kind;
         when TApp =>
            return Kinds.Right_Kind (This.Left.Get_Kind);
         when TGen =>
            return Kinds.Star;
      end case;
   end Get_Kind;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array
   is
   begin
      case This.Tag is
         when TVar =>
            return [This.Tyvar];
         when TCon =>
            return [];
         when TApp =>
            return Tyvars.Union
              (This.Left.Get_Tyvars,
               This.Right.Get_Tyvars);
         when TGen =>
            return [];
      end case;
   end Get_Tyvars;

   ----------
   -- Head --
   ----------

   function Head
     (This : Instance)
      return Conid
   is
   begin
      case This.Tag is
         when TVar =>
            raise Constraint_Error with "precondition false";
         when TCon =>
            return This.Tycon.Id;
         when TApp =>
            return This.Left.Head;
         when TGen =>
            raise Constraint_Error with "precondition false";
      end case;
   end Head;

   ----------------------
   -- Head_Normal_Form --
   ----------------------

   function Head_Normal_Form
     (This : Instance'Class)
      return Boolean
   is
   begin
      case This.Tag is
         when TVar =>
            return True;
         when TCon =>
            return False;
         when TApp =>
            return This.Left.Head_Normal_Form;
         when TGen =>
            return True;
      end case;
   end Head_Normal_Form;

   -----------------
   -- Instantiate --
   -----------------

   function Instantiate
     (This : not null access constant Instance'Class;
      Refs : Type_Array)
      return Reference
   is
   begin
      case This.Tag is
         when TVar =>
            return Reference (This);
         when TCon =>
            return Reference (This);
         when TApp =>
            return Application
              (This.Left.Instantiate (Refs),
               This.Right.Instantiate (Refs));
         when TGen =>
            return Refs (This.Index);
      end case;
   end Instantiate;

   -------------
   -- List_Of --
   -------------

   function List_Of
     (This : not null access constant Instance'Class)
      return Reference
   is
   begin
      return Application (T_List, Reference (This));
   end List_Of;

   -----------
   -- Match --
   -----------

   function Match
     (Left, Right : not null access constant Instance'Class;
      Success     : out Boolean)
      return Leander.Core.Substitutions.Instance
   is
   begin
      if Left.Tag /= TVar and then Left.Tag /= Right.Tag then
         Success := False;
         return Leander.Core.Substitutions.Empty;
      end if;

      case Left.Tag is
         when TVar =>
            Success := True;
            return Leander.Core.Substitutions.Singleton
              (Leander.Names.Leander_Name (Left.Tyvar.Name), Right);
         when TCon =>
            Success := Left.Tycon.Id = Right.Tycon.Id;
            return Leander.Core.Substitutions.Empty;
         when TApp =>
            declare
               Left_Subst  : constant Leander.Core.Substitutions.Instance :=
                               Match (Left.Left, Right.Left, Success);
            begin
               if Success then
                  declare
                     Right_Subst : constant Leander.Core.Substitutions.Instance :=
                                     Match (Left.Right, Right.Right, Success);
                  begin
                     if Success then
                        return Left_Subst.Merge (Right_Subst, Success);
                     else
                        return Leander.Core.Substitutions.Empty;
                     end if;
                  end;
               end if;
            end;
         when TGen =>
            null;
      end case;
      Success := False;
      return Leander.Core.Substitutions.Empty;
   end Match;

   --------------
   -- New_TVar --
   --------------

   function New_TVar return Reference is
   begin
      return TVar (Tyvars.New_Tyvar);
   end New_TVar;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      --  Types now live in the ADR 0001 arena; permanent types are retained
      --  for the life of the process and scratch types are reclaimed by
      --  End_Scratch.  There is nothing to sweep here.
      null;
   end Prune;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
   begin
      case This.Tag is
         when TVar =>
            return This.Tyvar.Show;
         when TCon =>
            return This.Tycon.Show;
         when TGen =>
            return [Character'Val (Character'Pos ('a') + This.Index - 1)];
         when TApp =>
            if This.Left = T_List then
               return "[" & This.Right.Show & "]";
            else
               declare
                  Left : Instance renames Instance (This.Left.all);
               begin
                  if Left.Tag = TApp then
                     if Left.Left = T_Arrow then
                        if Left.Right.Tag = TApp
                          and then Left.Right.Left.Tag = TApp
                          and then Left.Right.Left.Left = T_Arrow
                        then
                           return "(" & Left.Right.Show & ")"
                             & " -> "
                             & This.Right.Show;
                        else
                           return Left.Right.Show
                             & " -> "
                             & This.Right.Show;
                        end if;
                     elsif Left.Left.Show = "(,)" then
                        return "("
                          & Left.Right.Show
                          & ","
                          & This.Right.Show
                          & ")";
                     elsif This.Right.Tag = TApp then
                        return This.Left.Show
                          & " ("
                          & This.Right.Show
                          & ")";
                     else
                        return This.Left.Show & " " & This.Right.Show;
                     end if;
                  else
                     return This.Left.Show & " " & This.Right.Show;
                  end if;
               end;
            end if;
      end case;
   end Show;

   ----------
   -- TCon --
   ----------

   function TCon (T : Tycons.Instance) return Reference is
   begin
      return Allocate (Instance'(TCon, T));
   end TCon;

   ----------
   -- TGen --
   ----------

   function TGen (Index : Positive) return Reference is
   begin
      return Allocate (Instance'(TGen, Index));
   end TGen;

   --------------
   -- Traverse --
   --------------

   overriding procedure Traverse
     (This    : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Traverseable.Abstraction'Class))
   is
   begin
      Process (This);
      case This.Tag is
         when TVar =>
            null;
         when TCon =>
            null;
         when TGen =>
            null;
         when TApp =>
            This.Left.Traverse (Process);
            This.Right.Traverse (Process);
      end case;
   end Traverse;

   ----------
   -- TVar --
   ----------

   function TVar (T : Tyvars.Instance) return Reference is
   begin
      return Allocate (Instance'(TVar, T));
   end TVar;

end Leander.Core.Types;
