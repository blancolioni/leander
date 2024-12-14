with Leander.Core.Bindings;
with Leander.Core.Expressions;
with Leander.Maybes;

package Leander.Core.Environment is

   type Abstraction is interface;
   type Reference is not null access constant Abstraction'Class;

   package Maybe_Binding is
     new Leander.Maybes (Leander.Core.Expressions.Reference);

   function Find
     (This : Abstraction;
      Id   : Name_Id)
      return Maybe_Binding.Maybe
      is abstract;

   function Empty_Environment return Reference;

   function Append
     (This    : Abstraction;
      Binding : Bindings.Reference)
      return Reference
      is abstract;

end Leander.Core.Environment;
