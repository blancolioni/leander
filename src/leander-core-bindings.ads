limited with Leander.Core.Expressions;
with Leander.Showable;

package Leander.Core.Bindings is

   type Abstraction is interface
     and Leander.Showable.Abstraction;

   type Reference is not null access constant Abstraction'Class;

   function Id (This : Abstraction) return Name_Id is abstract;
   function Binding
     (This : Abstraction)
      return not null access constant Expressions.Abstraction'Class
   is abstract;

   function Bind
     (Id    : Name_Id;
      Value : not null access constant Expressions.Abstraction'Class)
      return Reference;

end Leander.Core.Bindings;
