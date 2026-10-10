with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Leander.Calculus;
with Leander.Core.Inference;
with Leander.Core.Predicates;
with Leander.Data_Types;
with Leander.Environment;

package Leander.Core.Alts.Compiler is

   type Builder is tagged private;

   procedure Initialize
     (This      : in out Builder'Class;
      Context   : Leander.Core.Inference.Inference_Context'Class;
      Env       : Leander.Environment.Reference);

   procedure Add_Name
     (This : in out Builder'Class;
      Name : Varid);

   procedure Set_Failure_Message
     (This    : in out Builder'Class;
      Message : String);
   --  What a value that no alternative matches reports, such as
   --  "f.hs:3:7: Non-exhaustive patterns in f".

   function Raise_Error (Message : String) return Leander.Calculus.Tree;
   --  A tree that raises Message at run time.  The message goes to the
   --  machine a character at a time (#errorChar), then #errorRaise raises
   --  it: an evaluated string cannot cross the primitive interface whole.

   procedure Add (This : in out Builder'Class;
                  Alts : Reference_Array)
     with Pre => Alts'Length > 0;

   function To_Calculus
     (This : in out Builder'Class)
      return Leander.Calculus.Tree;

   function Raised_Predicates
     (This : Builder'Class)
      return Leander.Core.Predicates.Predicate_Array;

private

   type Nullable_Pattern is
     access constant Leander.Core.Patterns.Instance'Class;
   type Nullable_Expression is
     access constant Leander.Core.Expressions.Instance'Class;

   type Con_Pat_Expr is
      record
         Pat   : Nullable_Pattern;
         Expr  : Nullable_Expression;
      end record;

   package Con_Pat_Expr_Vectors is
     new Ada.Containers.Vectors (Positive, Con_Pat_Expr);

   package Varid_Vectors is
     new Ada.Containers.Vectors (Positive, Varid);

   type Builder is tagged
      record
         Context      : Leander.Core.Inference.Inference_Context;
         Env          : Leander.Environment.Reference;
         Names        : Varid_Vectors.Vector;
         Compare_Mode : Boolean := False;
         Newtype_Mode : Boolean := False;
         DT           : Leander.Data_Types.Reference;
         Con_Pats     : Con_Pat_Expr_Vectors.Vector;
         Con_Dfl      : Con_Pat_Expr;
         Failure      : Ada.Strings.Unbounded.Unbounded_String :=
                          Ada.Strings.Unbounded.To_Unbounded_String
                            ("Non-exhaustive patterns");
      end record;

   function Raised_Predicates
     (This : Builder'Class)
      return Leander.Core.Predicates.Predicate_Array
   is (This.Context.Current_Predicates);

end Leander.Core.Alts.Compiler;
