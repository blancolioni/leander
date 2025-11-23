with Leander.Calculus;
limited with Leander.Core.Binding_Groups;
with Leander.Core.Inference;
with Leander.Core.Literals;
with Leander.Core.Qualified_Types;
with Leander.Core.Typeable;
with Leander.Disposable;
limited with Leander.Environment;
with Leander.Showable;
with Leander.Source;
with Leander.Traverseable;

package Leander.Core.Expressions is

   type Instance (<>) is
     new Leander.Showable.Abstraction
     and Leander.Core.Qualified_Types.Has_Qualified_Type
     and Leander.Core.Typeable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Source.Has_Source_Location
     and Leander.Traverseable.Abstraction
   with private;

   type Reference is not null access all Instance'Class;

   function Variable
     (Loc : Leander.Source.Source_Location;
      Id  : Varid)
      return Reference;

   function Constructor
     (Loc     : Leander.Source.Source_Location;
      Id      : Conid)
      return Reference;

   function Literal
     (Loc : Leander.Source.Source_Location;
      Lit : Literals.Instance)
      return Reference;

   function Application
     (Loc         : Leander.Source.Source_Location;
      Left, Right : Reference)
      return Reference;

   function Lambda
     (Loc        : Leander.Source.Source_Location;
      Id         : Varid;
      Expression : Reference)
      return Reference;

   function Let
     (Loc      : Leander.Source.Source_Location;
      Bindings : Leander.Core.Binding_Groups.Reference;
      Expr     : Reference)
      return Reference;

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean;

   function To_Calculus
     (This : Instance'Class;
      Types : Leander.Core.Inference.Inference_Context'Class;
      Env   : not null access constant Leander.Environment.Abstraction'Class)
      return Leander.Calculus.Tree;

   procedure Prune;

private

   type Instance_Tag is (ELit, EVar, ECon, EApp, ELam, ELet);

   type Binding_Group_Reference is
     access constant Leander.Core.Binding_Groups.Instance'Class;

   type Nullable_Qualified_Type_Reference is
     access constant Leander.Core.Qualified_Types.Instance'Class;

   type Instance (Tag : Instance_Tag) is
     new Leander.Showable.Abstraction
     and Leander.Core.Qualified_Types.Has_Qualified_Type
     and Leander.Core.Typeable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Source.Has_Source_Location
     and Leander.Traverseable.Abstraction with
      record
         Id : Core.Typeable.Typeable_Id;
         Loc : Leander.Source.Source_Location;
         QT  : Nullable_Qualified_Type_Reference;
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

   overriding function Location
     (This : Instance)
      return Leander.Source.Source_Location
   is (This.Loc);

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

   overriding procedure Update_Traverse
     (This    : not null access Instance;
      Process : not null access
        procedure (This : not null access
                     Leander.Traverseable.Abstraction'Class));

   overriding function Has_Qualified_Type_Value
     (This : Instance)
      return Boolean
   is (This.QT /= null);

   overriding function Qualified_Type
     (This : Instance)
      return Leander.Core.Qualified_Types.Reference
   is (Leander.Core.Qualified_Types.Reference (This.QT));

   overriding procedure Set_Qualified_Type
     (This : in out Instance;
      QT   : Leander.Core.Qualified_Types.Reference);

   function Allocate
     (This : Instance)
      return Reference;

end Leander.Core.Expressions;
