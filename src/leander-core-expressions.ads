with Leander.Allocation;
with Leander.Core.Binding_Groups;
with Leander.Core.Literals;
with Leander.Core.Types;
with Leander.Showable;

package Leander.Core.Expressions is

   type Abstraction is interface
     and Leander.Showable.Abstraction
     and Leander.Allocation.Abstraction;

   type Reference is not null access constant Abstraction'Class;

   function Variable (Id : Name_Id) return Reference;
   function Constructor
     (Id : Name_Id;
      CType : Types.Reference)
      return Reference;
   function Literal (Lit : Literals.Reference) return Reference;
   function Apply (Left, Right : Reference) return Reference;
   function Lambda (Id : Name_Id;
                    Expression : Reference)
                    return Reference;

   function Let (Bindings    : Core.Binding_Groups.Reference;
                 Expression  : Reference)
                 return Reference;

   type Expression_Visitor is interface;

   procedure Visit
     (This    : not null access constant Abstraction;
      Visitor : in out Expression_Visitor'class)
   is abstract;

   procedure Variable
     (This : in out Expression_Visitor;
      Id   : Name_Id)
   is null;

   procedure Constructor
     (This  : in out Expression_Visitor;
      Id    : Name_Id;
      CType : Types.Reference)
   is null;

   procedure Literal
     (This : in out Expression_Visitor;
      Literal : Literals.Reference)
   is null;

   procedure Application
     (This    : in out Expression_Visitor;
      F, X    : Reference)
   is null;

   procedure Lambda
     (This    : in out Expression_Visitor;
      X       : Name_Id;
      E       : Reference)
   is null;

   procedure Let
     (This       : in out Expression_Visitor;
      Bindings   : Core.Binding_Groups.Reference;
      Expression : Reference)
   is null;

end Leander.Core.Expressions;
