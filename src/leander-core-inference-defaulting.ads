with Leander.Core.Predicates;
with Leander.Core.Tyvars;

package Leander.Core.Inference.Defaulting is

   procedure Default_Ambiguous
     (Context    : in out Inference_Context'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Keep       : Leander.Core.Tyvars.Tyvar_Array);
   --  Resolve the ambiguous type variables of Predicates by defaulting
   --  (Haskell 2010, section 4.3.4).  A type variable is ambiguous when no
   --  type will ever fix it: it is not in Keep, which holds the variables
   --  of the binding's type and of the environment.  It is defaulted when
   --  every predicate on it has the form C v, at least one C is a numeric
   --  class, and all of them are standard classes: it becomes the first
   --  of the default types (Int, for now) that is an instance of them
   --  all.  Defaulting unifies it in Context, so the predicates are then
   --  resolved like any other at that type.  Anything else is left alone.

   procedure Check_Instances
     (Context    : in out Inference_Context'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array);
   --  Report, as an error in Context, each of Predicates that has no type
   --  variable left and no instance: Num Bool, from 1 + True.  Nothing
   --  can resolve it later.  Without the classes to ask, nothing is
   --  checked.

end Leander.Core.Inference.Defaulting;
