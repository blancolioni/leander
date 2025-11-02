with Ada.Containers;
with Ada.Containers.Vectors;
with Leander.Calculus;
with Leander.Core.Inference;
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

   procedure Add (This : in out Builder'Class;
                  Alts : Reference_Array)
     with Pre => Alts'Length > 0
     and then Alts (Alts'First).Patterns'Length > 0;

   function To_Calculus
     (This : Builder'Class)
      return Leander.Calculus.Tree;

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
         Context  : Leander.Core.Inference.Inference_Context;
         Env      : Leander.Environment.Reference;
         Names    : Varid_Vectors.Vector;
         Con_Pats : Con_Pat_Expr_Vectors.Vector;
         Con_Dfl  : Con_Pat_Expr;
      end record;

end Leander.Core.Alts.Compiler;
