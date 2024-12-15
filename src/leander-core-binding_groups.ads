with Leander.Core.Bindings;
with Leander.Showable;

package Leander.Core.Binding_Groups is

   type Abstraction is interface
     and Leander.Showable.Abstraction;

   type Reference is not null access constant Abstraction'Class;

   function Implicit_Bindings
     (This : Abstraction)
      return Bindings.Container_Array
      is abstract;

   function Binding_Group
     (Implicit_Bindings : Bindings.Container_Array)
      return Reference;

end Leander.Core.Binding_Groups;
