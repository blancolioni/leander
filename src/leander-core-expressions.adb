with Ada.Exceptions;

with Leander.Allocator;
with Leander.Core.Binding_Groups;
with Leander.Environment;
with Leander.Logging;

package body Leander.Core.Expressions is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("expressions", Instance, Variable_Reference);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance)
      return Reference
   is
   begin
      return Reference (Allocator.Allocate (This));
   end Allocate;

   -----------------
   -- Application --
   -----------------

   function Application
     (Loc         : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference
   is
   begin
      return Allocate
        (Instance'(EApp, Core.Typeable.New_Id, Loc, null, Left, Right));
   end Application;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (Loc     : Leander.Source.Source_Location;
      Id      : Conid)
      return Reference
   is
   begin
      return Allocate ((ECon, Core.Typeable.New_Id, Loc, null, Id));
   end Constructor;

   -------------
   -- Dispose --
   -------------

   overriding procedure Dispose (This : in out Instance) is
   begin
      null;
   end Dispose;

   -------------------
   -- Has_Reference --
   -------------------

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean
   is
   begin
      case This.Tag is
         when EVar =>
            return This.Var_Id = To;
         when ECon =>
            return False;
         when ELit =>
            return False;
         when EApp =>
            return This.Left.Has_Reference (To)
              or else This.Right.Has_Reference (To);
         when ELam =>
            return This.LVar /= To
              and then This.LBody.Has_Reference (To);
         when ELet =>
            return This.Let_Bindings.Has_Reference (To)
              or else This.Let_Body.Has_Reference (To);
      end case;
   end Has_Reference;

   ------------
   -- Lambda --
   ------------

   function Lambda (Loc        : Leander.Source.Source_Location;
      Id         : Varid;
      Expression : Reference)
                    return Reference
   is
   begin
      return Allocate
        ((ELam, Core.Typeable.New_Id, Loc, null, Id, Expression));
   end Lambda;

   ---------
   -- Let --
   ---------

   function Let
     (Loc      : Leander.Source.Source_Location;
      Bindings : Leander.Core.Binding_Groups.Reference;
      Expr     : Reference)
      return Reference
   is
      Ref : constant Binding_Group_Reference :=
              Binding_Group_Reference (Bindings);
   begin
      return Allocate ((ELet, Core.Typeable.New_Id, Loc, null, Ref, Expr));
   end Let;

   -------------
   -- Literal --
   -------------

   function Literal (Loc : Leander.Source.Source_Location;
      Lit : Literals.Instance)
                     return Reference is
   begin
      return Allocate ((ELit, Core.Typeable.New_Id, Loc, null, Lit));
   end Literal;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

   ------------------------
   -- Set_Qualified_Type --
   ------------------------

   overriding procedure Set_Qualified_Type
     (This : in out Instance;
      QT   : Leander.Core.Qualified_Types.Reference)
   is
   begin
      This.QT := Nullable_Qualified_Type_Reference (QT);
   end Set_Qualified_Type;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
   begin
      case This.Tag is
         when EVar =>
            return To_String (This.Var_Id);
         when ECon =>
            return To_String (This.Con_Id);
         when ELit =>
            return This.Literal.Show;
         when EApp =>
            declare
               function Paren (Img : String; P : Boolean) return String
               is (if P then "(" & Img & ")" else Img);

               Left_Image : constant String :=
                              Paren (This.Left.Show,
                                     This.Left.Tag in ELam | ELet);

               Right_Image : constant String :=
                               Paren (This.Right.Show,
                                      This.Right.Tag in EApp | ELam | ELet);
            begin
               return Left_Image & " " & Right_Image;
            end;
         when ELam =>
            return "\" & To_String (This.LVar) & " -> "
              & This.LBody.Show;
         when ELet =>
            return "let {" & This.Let_Bindings.Show
              & "} in " & This.Let_Body.Show;
      end case;
   end Show;

   -----------------
   -- To_Calculus --
   -----------------

   function To_Calculus
     (This : Instance'Class;
      Types : Leander.Core.Inference.Inference_Context'Class;
      Env   : not null access constant Leander.Environment.Abstraction'Class)
      return Leander.Calculus.Tree
   is
      use Leander.Calculus;
      Result : Tree;
   begin
      case This.Tag is
         when EVar =>
            Leander.Logging.Log
              ("COMPILE",
               This.Show & " :: "
               & This.Qualified_Type.Show);
            declare
               E : Tree :=
                     Symbol (Leander.Names.Leander_Name (This.Var_Id));
            begin
               for P of This.Qualified_Type.Predicates loop
                  E := Apply (E, Symbol ("<" & P.Show & ">"));
               end loop;
               Result := E;
            end;
         when ECon =>
            Result := Env.Constructor (Leander.Names.Leander_Name (This.Con_Id));
         when ELit =>
            Result := This.Literal.To_Calculus;
         when EApp =>
            Result := Apply
              (This.Left.To_Calculus (Types, Env),
               This.Right.To_Calculus (Types, Env));
         when ELam =>
            Result := Lambda
              (To_String (This.LVar),
               This.LBody.To_Calculus (Types, Env));
         when ELet =>
            declare
               E : Tree := This.Let_Body.To_Calculus (Types, Env);
               Ids : constant Varid_Array :=
                       This.Let_Bindings.Varids;
            begin
               for Id of Ids loop
                  E := Lambda (Leander.Names.Leander_Name (Id), E);
                  E := Apply
                    (E,
                     This.Let_Bindings.Lookup
                       (Leander.Names.Leander_Name (Id))
                     .To_Calculus (Types, Env));
               end loop;
               Result := E;
            end;
      end case;
      Leander.Logging.Log
        ("RESULT",
         To_String (Result));
      return Result;

   exception
      when E : others =>
         This.Error (Ada.Exceptions.Exception_Message (E));
         raise;
   end To_Calculus;

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
         when EVar =>
            null;
         when ECon =>
            null;
         when ELit =>
            null;
         when EApp =>
            This.Left.Traverse (Process);
            This.Right.Traverse (Process);
         when ELam =>
            This.LBody.Traverse (Process);
         when ELet =>
            for Id of This.Let_Bindings.Varids loop
               for Alt of
                 This.Let_Bindings.Lookup (Leander.Names.Leander_Name (Id))
                   .Alts
               loop
                  Alt.Traverse (Process);
               end loop;
            end loop;
            This.Let_Body.Traverse (Process);
      end case;
   end Traverse;

   ---------------------
   -- Update_Traverse --
   ---------------------

   overriding procedure Update_Traverse
     (This    : not null access Instance;
      Process : not null access
        procedure (This : not null access
                     Leander.Traverseable.Abstraction'Class))
   is
   begin
      Process (This);
      case This.Tag is
         when EVar =>
            null;
         when ECon =>
            null;
         when ELit =>
            null;
         when EApp =>
            This.Left.Update_Traverse (Process);
            This.Right.Update_Traverse (Process);
         when ELam =>
            This.LBody.Update_Traverse (Process);
         when ELet =>
            for Id of This.Let_Bindings.Varids loop
               for Alt of
                 This.Let_Bindings.Lookup (Leander.Names.Leander_Name (Id))
                   .Alts
               loop
                  Alt.Update_Traverse (Process);
               end loop;
            end loop;
            This.Let_Body.Update_Traverse (Process);
      end case;
   end Update_Traverse;

   --------------
   -- Variable --
   --------------

   function Variable (Loc : Leander.Source.Source_Location;
      Id  : Varid)
                      return Reference is
   begin
      return Allocate (Instance'(EVar, Core.Typeable.New_Id, Loc, null, Id));
   end Variable;

end Leander.Core.Expressions;
