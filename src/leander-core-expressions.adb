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
        (Instance'(EApp, Core.Typeable.New_Id, Loc, Left, Right));
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
      return Allocate ((ECon, Core.Typeable.New_Id, Loc, Id));
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
      return Allocate ((ELam, Core.Typeable.New_Id, Loc, Id, Expression));
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
      return Allocate ((ELet, Core.Typeable.New_Id, Loc, Ref, Expr));
   end Let;

   -------------
   -- Literal --
   -------------

   function Literal (Loc : Leander.Source.Source_Location;
      Lit : Literals.Instance)
                     return Reference is
   begin
      return Allocate ((ELit, Core.Typeable.New_Id, Loc, Lit));
   end Literal;

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
   begin
      Leander.Logging.Log
        ("CALC", This.Show);
      case This.Tag is
         when EVar =>
            return Symbol (Leander.Names.Leander_Name (This.Var_Id));
         when ECon =>
            return Env.Constructor (Leander.Names.Leander_Name (This.Con_Id));
         when ELit =>
            return This.Literal.To_Calculus;
         when EApp =>
            return Apply
              (This.Left.To_Calculus (Types, Env),
               This.Right.To_Calculus (Types, Env));
         when ELam =>
            return Lambda
              (To_String (This.LVar),
               This.LBody.To_Calculus (Types, Env));
         when ELet =>
            declare
               E : Tree := This.Let_Body.To_Calculus (Types, Env);
               Ids : constant Varid_Array :=
                       This.Let_Bindings.Varids;
            begin
               for Id of reverse Ids loop
                  E := Lambda (Leander.Names.Leander_Name (Id), E);
               end loop;
               for Id of Ids loop
                  E := Apply
                    (E,
                     This.Let_Bindings.Lookup
                       (Leander.Names.Leander_Name (Id))
                     .To_Calculus (Types, Env));
               end loop;
               return E;
            end;
      end case;
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
            null;
      end case;
   end Traverse;

   --------------
   -- Variable --
   --------------

   function Variable (Loc : Leander.Source.Source_Location;
      Id  : Varid)
                      return Reference is
   begin
      return Allocate (Instance'(EVar, Core.Typeable.New_Id, Loc, Id));
   end Variable;

end Leander.Core.Expressions;
