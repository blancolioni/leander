limited with Leander.Core.Expressions;
with Leander.Showable;

package Leander.Core.Bindings is

   type Abstraction is interface
     and Leander.Showable.Abstraction;
   type Reference is not null access constant Abstraction'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   function Id (This : Abstraction) return Name_Id is abstract;
   function Binding
     (This : Abstraction)
      return not null access constant Expressions.Abstraction'Class
   is abstract;

   function Bind
     (Id    : Name_Id;
      Value : not null access constant Expressions.Abstraction'Class)
      return Reference;

   type Container_Abstraction is interface and Leander.Showable.Abstraction;
   type Container_Reference is access constant Container_Abstraction'Class;
   type Container_Array is array (Positive range <>) of Container_Reference;

   function To_Array (This : Container_Abstraction) return Reference_Array
                      is abstract;

   function Container
     (Bindings : Reference_Array)
      return Container_Reference;

end Leander.Core.Bindings;
