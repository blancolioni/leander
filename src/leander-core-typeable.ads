with Leander.Showable;
with Leander.Traverseable;

package Leander.Core.Typeable is

   type Typeable_Id is private;

   type Abstraction is interface
     and Leander.Showable.Abstraction
     and Leander.Traverseable.Abstraction;

   function Get_Id (This : Abstraction) return Typeable_Id
                    is abstract;

   function New_Id return Typeable_Id;

private

   type Typeable_Id is new Positive;

end Leander.Core.Typeable;
