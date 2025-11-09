with Leander.Allocator;
with Leander.Names;

package body Leander.Core.Types is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("types", Instance, Variable_Reference);

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
      return Reference (Allocator.Allocate (Instance (This)));
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

   --------------
   -- Generate --
   --------------

   function Generate
     (This : not null access constant Instance'Class)
      return Reference
   is
      Tvs : constant Core.Tyvars.Tyvar_Array := This.Get_Tyvars;
      Gens : constant Type_Array :=
               [for I in Tvs'Range => Core.Types.TGen (I)];

      function Create_Subst
        (Index : Positive)
         return Leander.Core.Substitutions.Instance;

      ------------------
      -- Create_Subst --
      ------------------

      function Create_Subst
        (Index : Positive)
         return Leander.Core.Substitutions.Instance
      is
      begin
         if Index <= Tvs'Length then
            return Leander.Core.Substitutions.Compose
              (Leander.Names.Leander_Name (Tvs (Index).Name),
               Gens (Index),
               Create_Subst (Index + 1));
         else
            return Leander.Core.Substitutions.Empty;
         end if;
      end Create_Subst;

   begin
      return This.Apply (Create_Subst (1));
   end Generate;

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
      Allocator.Prune;
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
                             & "->"
                             & This.Right.Show;
                        else
                           return Left.Right.Show
                             & "->"
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
     (This : not null access constant Instance;
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
