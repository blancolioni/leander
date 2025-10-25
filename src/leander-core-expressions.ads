limited with Leander.Core.Binding_Groups;
with Leander.Core.Literals;
with Leander.Core.Typeable;
with Leander.Disposable;
with Leander.Showable;
with Leander.Traverseable;

package Leander.Core.Expressions is

   type Instance (<>) is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Traverseable.Abstraction
   with private;

   type Reference is not null access constant Instance'Class;

   function Variable (Id : Varid) return Reference;

   function Constructor
     (Id      : Conid)
      return Reference;

   function Literal (Lit : Literals.Instance) return Reference;

   function Application (Left, Right : Reference) return Reference;

   function Lambda (Id         : Varid;
                    Expression : Reference)
                    return Reference;

   function Let
     (Bindings : Leander.Core.Binding_Groups.Reference;
      Expr     : Reference)
      return Reference;

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean;

   procedure Prune;

private

   type Instance_Tag is (ELit, EVar, ECon, EApp, ELam, ELet);

   type Binding_Group_Reference is
     access constant Leander.Core.Binding_Groups.Instance'Class;

   type Instance (Tag : Instance_Tag) is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Traverseable.Abstraction with
      record
         Id : Core.Typeable.Typeable_Id;
         case Tag is
            when EVar =>
               Var_Id      : Varid;
            when ECon =>
               Con_Id      : Conid;
            when ELit =>
               Literal     : Leander.Core.Literals.Instance;
            when EApp =>
               Left, Right : Reference;
            when ELam =>
               LVar        : Varid;
               LBody       : Reference;
            when ELet =>
               Let_Bindings : Binding_Group_Reference;
               Let_Body     : Reference;
         end case;
      end record;

   overriding function Show
     (This : Instance)
      return String;

   overriding procedure Dispose (This : in out Instance);

   overriding function Get_Id
     (This : Instance)
      return Leander.Core.Typeable.Typeable_Id
   is (This.Id);

   overriding procedure Traverse
     (This    : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Traverseable.Abstraction'Class));

   function Allocate
     (This : Instance)
      return Reference;

end Leander.Core.Expressions;
