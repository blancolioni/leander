with Leander.Calculus;
with Leander.Core.Alts;
with Leander.Core.Inference;
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

   function Implicit_Binding
     (Name : Varid;
      Alts : Leander.Core.Alts.Reference_Array)
      return Reference;

   function Explicit_Binding
     (Name   : Varid;
      Alts   : Leander.Core.Alts.Reference_Array;
      Scheme : Leander.Core.Schemes.Reference)
      return Reference;

   function To_Calculus
     (This  : Instance'Class;
      Types : Leander.Core.Inference.Inference_Context'Class;
      Env   : not null access constant Leander.Environment.Abstraction'Class)
      return Leander.Calculus.Tree;

   procedure Update_Type
     (This    : Instance'Class;
      Context : Leander.Core.Inference.Inference_Context);

   procedure Prune;

private

   type Nullable_Scheme_Reference is
     access constant Leander.Core.schemes.Instance'Class;

   type Instance (Alt_Count : Natural) is
     new Leander.Showable.Abstraction with
      record
         Name   : Varid;
         Alts   : Leander.Core.Alts.Reference_Array (1 .. Alt_Count);
         Scheme : Nullable_Scheme_Reference;
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

end Leander.Core.Bindings;
