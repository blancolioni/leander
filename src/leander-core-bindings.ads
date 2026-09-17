with Leander.Calculus;
with Leander.Core.Alts;
with Leander.Core.Inference;
with Leander.Core.Predicates;
with Leander.Core.Schemes;
limited with Leander.Environment;
with Leander.Showable;

package Leander.Core.Bindings is

   type Instance (<>) is
     new Leander.Showable.Abstraction
   with private;

   type Reference is access constant Instance'Class;

   type Reference_Array is array (Positive range <>) of Reference;

   function Name (This : Instance) return Varid;

   function Alts
     (This : Instance)
      return Leander.Core.Alts.Reference_Array;

   function Scheme
     (This : Instance)
      return Leander.Core.Schemes.Reference;

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean;

   function Monomorphic (This : Instance) return Boolean;
   --  True for a binding that must not be generalised.  Set on the synthetic
   --  bindings the parser creates when desugaring case/if/do-pattern-binds:
   --  each is applied at exactly one site, so quantifying it buys no
   --  polymorphism, and quantifying it *severs* the body's type variables
   --  from that site -- which strands any class constraint arising in the
   --  body, since an implicit binding's scheme carries no predicates.

   procedure Set_Dictionaries
     (This : Instance'Class;
      Ps   : Leander.Core.Predicates.Predicate_Array);
   --  Record the predicates this binding's scheme retains, in scheme order.
   --  Generalisation calls this; elaboration turns each one into a
   --  dictionary lambda parameter, in the order a use site applies them.

   function Dictionaries
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array;

   function Implicit_Binding
     (Name        : Varid;
      Alts        : Leander.Core.Alts.Reference_Array;
      Monomorphic : Boolean := False)
      return Reference;

   function Explicit_Binding
     (Name   : Varid;
      Alts   : Leander.Core.Alts.Reference_Array;
      Scheme : Leander.Core.Schemes.Reference)
      return Reference;

   function To_Calculus
     (This  : Instance'Class;
      Types : in out Leander.Core.Inference.Inference_Context'Class;
      Env   : not null access constant Leander.Environment.Abstraction'Class)
      return Leander.Calculus.Tree;

   procedure Update_Type
     (This    : Instance'Class;
      Context : Leander.Core.Inference.Inference_Context);

   procedure Prune;

   procedure Report;

private

   type Nullable_Scheme_Reference is
     access constant Leander.Core.schemes.Instance'Class;

   type Instance (Alt_Count : Natural) is
     new Leander.Showable.Abstraction with
      record
         Name        : Varid;
         Alts        : Leander.Core.Alts.Reference_Array (1 .. Alt_Count);
         Scheme      : Nullable_Scheme_Reference;
         Monomorphic : Boolean := False;
         Dict_Id     : Positive;
         --  Key into the body's dictionary table.  The predicates live
         --  there rather than in this record so that recording them does
         --  not need a variable view of an access-constant Reference.
      end record;

   overriding function Show (This : Instance) return String;

   function Name (This : Instance) return Varid
   is (This.Name);

   function Alts
     (This : Instance)
      return Leander.Core.Alts.Reference_Array
   is (This.Alts);

   function Scheme
     (This : Instance)
      return Leander.Core.Schemes.Reference
   is (Leander.Core.Schemes.Reference (This.Scheme));

   function Monomorphic (This : Instance) return Boolean
   is (This.Monomorphic);

end Leander.Core.Bindings;
