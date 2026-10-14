with Leander.Core.Bindings;
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
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Binding    : Leander.Core.Bindings.Reference := null);
   --  Report the first of Predicates that nothing can ever satisfy: one
   --  with no type variable left and no instance, such as Num Bool from
   --  1 + True, or one whose type's head is a constructor no instance
   --  matches, such as Show (a -> b) from show map (#76).  It is reported
   --  against Binding, the binding whose inference raised it, when there
   --  is one, and Context rejects the program: inference itself is still
   --  sound, so later bindings are not blamed for it (#157).  Without the
   --  classes to ask, nothing is checked.

end Leander.Core.Inference.Defaulting;
