package body Leander.Core.Types is

   Next_TVar : Positive := 1;

   type Instance_Class is
     (TVar, TCon, TAp, TGen);

   subtype Parent is Abstraction;

   type Instance (Class : Instance_Class) is new Parent with
      record
         case Class is
            when TVar =>
               Var         : Tyvars.Reference;
            when TCon =>
               Con         : Tycons.Reference;
            when TAp =>
               Left, Right : Reference;
            when TGen =>
               Index       : Positive;
         end case;
      end record;

   overriding function Kind
     (This : Instance)
      return Kinds.Reference;

   overriding function Show
     (This : Instance)
      return String;

   overriding function Application
     (Left  : not null access constant Instance;
      Right : not null access constant Abstraction'Class)
      return Reference;

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : not null access constant Substitutions.Abstraction'Class)
      return Reference;

   overriding procedure Visit
     (This    : not null access constant Instance;
      Visitor : in out Type_Visitor'class);

   overriding function Contains
     (This  : Instance;
      Tyvar : Tyvars.Reference)
      return Boolean
   is (case This.Class is
          when TVar => This.Var.Name = Tyvar.Name,
          when TCon => False,
          when TGen => False,
          when TAp  => This.Left.Contains (Tyvar)
       or else This.Right.Contains (Tyvar));

   overriding function Get_Tyvars
     (This  : Instance)
      return Tyvars.Tyvar_Array;

   overriding function Instantiate
     (This : not null access constant Instance;
      Refs : Reference_Array)
      return Reference;

   overriding function Is_Variable
     (This : Instance)
      return Boolean
   is (This.Class = TVar);

   overriding function Variable
     (This : Instance)
      return Tyvars.Reference
   is (This.Var);

   overriding function Is_Constructor
     (This : Instance)
      return Boolean
   is (This.Class = TCon);

   overriding function Constructor
     (This : Instance)
      return Tycons.Reference
   is (This.Con);

   overriding function Is_Application
     (This : Instance)
      return Boolean
   is (This.Class = TAp);

   overriding function Left
     (This : Instance)
      return Reference
   is (This.Left);

   overriding function Right
     (This : Instance)
      return Reference
   is (This.Right);

   function Allocate
     (This : Instance'Class)
      return Reference;

   package Static_Values is
      Local_Kind_Star_Star : constant Kinds.Reference :=
                               Kinds.KFun (Kinds.Star, Kinds.Star);

      Local_Kind_Star_Star_Star : constant Kinds.Reference :=
                                    Kinds.KFun (Kinds.Star,
                                                Local_Kind_Star_Star);

      Local_Unit_Con : constant Tycons.Reference :=
                         Tycons.Tycon (Id ("()"), Kinds.Star);

      Local_Error_Con : constant Tycons.Reference :=
                          Tycons.Tycon (Id ("#error"), Kinds.Star);

      Local_Char_Con : constant Tycons.Reference :=
                         Tycons.Tycon (Id ("Char"), Kinds.Star);

      Local_Int_Con : constant Tycons.Reference :=
                        Tycons.Tycon (Id ("Int"), Kinds.Star);

      Local_Integer_Con : constant Tycons.Reference :=
                            Tycons.Tycon (Id ("Integer"), Kinds.Star);

      Local_Float_Con : constant Tycons.Reference :=
                          Tycons.Tycon (Id ("Float"), Kinds.Star);

      Local_Double_Con : constant Tycons.Reference :=
                           Tycons.Tycon (Id ("Double"), Kinds.Star);

      Local_List_Con : constant Tycons.Reference :=
                         Tycons.Tycon (Id ("[]"), Local_Kind_Star_Star);

      Local_Arrow_Con : constant Tycons.Reference :=
                          Tycons.Tycon (Id ("(->)"),
                                        Local_Kind_Star_Star_Star);

      Local_Tuple2_Con : constant Tycons.Reference :=
                           Tycons.Tycon (Id ("(,)"),
                                         Local_Kind_Star_Star_Star);

      Local_TUnit    : aliased constant Instance := (TCon, Local_Unit_Con);
      Local_TError   : aliased constant Instance := (TCon, Local_Error_Con);
      Local_TChar    : aliased constant Instance := (TCon, Local_Char_Con);
      Local_TInt     : aliased constant Instance := (TCon, Local_Int_Con);
      Local_TInteger : aliased constant Instance := (TCon, Local_Integer_Con);
      Local_TFloat   : aliased constant Instance := (TCon, Local_Float_Con);
      Local_TDouble  : aliased constant Instance := (TCon, Local_Double_Con);
      Local_TList    : aliased constant Instance := (TCon, Local_List_Con);
      Local_TArrow   : aliased constant Instance := (TCon, Local_Arrow_Con);
      Local_TTuple2  : aliased constant Instance := (TCon, Local_Tuple2_Con);

      Local_TString  : aliased constant Instance :=
                         (TAp, Local_TList'Access, Local_TChar'Access);
   end Static_Values;

   function T_Arrow   return Reference
   is (Static_Values.Local_TArrow'Access);

   function T_Char   return Reference
   is (Static_Values.Local_TChar'Access);

   function T_Error   return Reference
   is (Static_Values.Local_TError'Access);

   function T_Int     return Reference
   is (Static_Values.Local_TInt'Access);

   function T_Integer return Reference
   is (Static_Values.Local_TInteger'Access);

   function T_Float   return Reference
   is (Static_Values.Local_TFloat'Access);

   function T_Double  return Reference
   is (Static_Values.Local_TDouble'Access);

   function T_List    return Reference
   is (Static_Values.Local_TList'Access);

   function T_Tuple_2 return Reference
   is (Static_Values.Local_TTuple2'Access);

   function T_Unit    return Reference
   is (Static_Values.Local_TUnit'Access);

   function T_String  return Reference is (Static_Values.Local_TString'Access);

   function Fn (From : not null access constant Abstraction'Class;
                To   : not null access constant Abstraction'Class)
                return Reference
   is (T_Arrow.Application (From).Application (To));

   function Pair (A, B : Reference) return Reference
   is (T_Tuple_2.Application (A).Application (B));

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance'Class)
      return Reference
   is
   begin
      return new Instance'Class'(This);
   end Allocate;

   -----------------
   -- Application --
   -----------------

   overriding function Application
     (Left  : not null access constant Instance;
      Right : not null access constant Abstraction'Class)
      return Reference
   is
   begin
      return Allocate (Instance'(TAp, Reference (Left), Reference (Right)));
   end Application;

   -----------
   -- Apply --
   -----------

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : not null access constant Substitutions.Abstraction'Class)
      return Reference
   is
   begin
      case This.Class is
         when TVar =>
            declare
               T : constant Substitutions.Maybe_Result.Maybe :=
                     Subst.Lookup (This.Var);
            begin
               if T.Is_Nothing then
                  return Reference (This);
               else
                  return Reference (T.From_Just);
               end if;
            end;
         when TCon =>
            return Reference (This);
         when TAp =>
               return This.Left.Apply (Subst).Application
                 (This.Right.Apply (Subst));
         when TGen =>
            return Reference (This);
      end case;
   end Apply;

   -----------
   -- Apply --
   -----------

   function Apply
     (Refs  : Reference_Array;
      Subst : not null access constant Substitutions.Abstraction'Class)
      return Reference_Array
   is
      Result : Reference_Array := Refs;
   begin
      for T of Result loop
         T := T.Apply (Subst);
      end loop;
      return Result;
   end Apply;

   ----------------
   -- Get_Tyvars --
   ----------------

   overriding function Get_Tyvars
     (This  : Instance)
      return Tyvars.Tyvar_Array
   is
   begin
      case This.Class is
         when TVar =>
            return [This.Var];
         when TCon =>
            return [];
         when TAp =>
            return Tyvars.Union
              (This.Left.Get_Tyvars,
               This.Right.Get_Tyvars);
         when TGen =>
            return [];
      end case;
   end Get_Tyvars;

   -----------------
   -- Instantiate --
   -----------------

   overriding function Instantiate
     (This : not null access constant Instance;
      Refs : Reference_Array)
      return Reference
   is
   begin
      case This.Class is
         when TVar =>
            return Reference (This);
         when TCon =>
            return Reference (This);
         when TAp =>
            return This.Left.Instantiate (Refs)
              .Application (This.Right.Instantiate (Refs));
         when TGen =>
            return Refs (This.Index);
      end case;
   end Instantiate;

   ----------
   -- Kind --
   ----------

   overriding function Kind
     (This : Instance)
      return Kinds.Reference
   is
   begin
      case This.Class is
         when TVar =>
            return This.Var.Kind;
         when TCon =>
            return This.Con.Kind;
         when TAp =>
            return This.Left.Kind.KAp;
         when TGen =>
            return Kinds.Star;
      end case;
   end Kind;

   ----------
   -- List --
   ----------

   function List
     (This : not null access constant Abstraction'Class)
      return Reference
   is
   begin
      return T_List.Application (This);
   end List;

   --------------
   -- New_TVar --
   --------------

   function New_TVar return Reference is
      Name : String := Next_TVar'Image;
   begin
      Next_TVar := Next_TVar + 1;
      Name (Name'First) := '_';
      return TVar
        (Tyvars.Tyvar (Id ("$t" & Name), Kinds.Star));
   end New_TVar;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
   begin
      case This.Class is
         when TVar =>
            return This.Var.Show;
         when TCon =>
            return This.Con.Show;
         when TGen =>
            declare
               Img : String := This.Index'Image;
            begin
               Img (Img'First) := '_';
               return Img;
            end;
         when TAp =>
            if This.Left = T_List then
               return "[" & This.Right.Show & "]";
            else
               declare
                  Left : Instance renames Instance (This.Left.all);
               begin
                  if Left.Class = TAp then
                     if Left.Left = T_Arrow then
                        return Left.Right.Show
                          & "->"
                          & This.Right.Show;
                     elsif Left.Left = T_Tuple_2 then
                        return "("
                          & Left.Right.Show
                          & ","
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

   function TCon (T : Tycons.Reference) return Reference is
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

   ----------
   -- TVar --
   ----------

   function TVar (T : Tyvars.Reference) return Reference is
   begin
      return Allocate (Instance'(TVar, T));
   end TVar;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This    : not null access constant Instance;
      Visitor : in out Type_Visitor'class)
   is
   begin
      case This.Class is
         when TVar =>
            Visitor.Variable (This.Var);
         when TCon =>
            Visitor.Constructor (This.Con);
         when TGen =>
            null;
         when TAp =>
            Visitor.Application (This.Left, This.Right);
      end case;
   end Visit;

end Leander.Core.Types;
