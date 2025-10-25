with Leander.Allocator;
with Leander.Core.Binding_Groups;

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
     (Left  : Reference;
      Right : Reference)
      return Reference
   is
   begin
      return Allocate
        (Instance'(EApp, Core.Typeable.New_Id, Left, Right));
   end Application;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (Id      : Conid)
      return Reference
   is
   begin
      return Allocate ((ECon, Core.Typeable.New_Id, Id));
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
            return False;
      end case;
   end Has_Reference;

   ------------
   -- Lambda --
   ------------

   function Lambda (Id : Varid;
                    Expression : Reference)
                    return Reference
   is
   begin
      return Allocate ((ELam, Core.Typeable.New_Id, Id, Expression));
   end Lambda;

   ---------
   -- Let --
   ---------

   function Let
     (Bindings : Leander.Core.Binding_Groups.Reference;
      Expr     : Reference)
      return Reference
   is
      Ref : constant Binding_Group_Reference :=
              Binding_Group_Reference (Bindings);
   begin
      return Allocate ((ELet, Core.Typeable.New_Id, Ref, Expr));
   end Let;

   -------------
   -- Literal --
   -------------

   function Literal (Lit : Literals.Instance) return Reference is
   begin
      return Allocate ((ELit, Core.Typeable.New_Id, Lit));
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
            null;      end case;
   end Traverse;

   --------------
   -- Variable --
   --------------

   function Variable (Id : Varid) return Reference is
   begin
      return Allocate (Instance'(EVar, Core.Typeable.New_Id, Id));
   end Variable;

end Leander.Core.Expressions;
