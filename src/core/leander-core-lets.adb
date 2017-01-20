with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;

package body Leander.Core.Lets is

   package Tree_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Core.Trees.Tree_Type, Leander.Core.Trees."=");

   package String_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, String);

   --------------------
   -- Let_Expression --
   --------------------

   function Let_Expression
     (Bindings   : Leander.Environments.Environment;
      Expression : Leander.Core.Trees.Tree_Type)
      return Leander.Core.Trees.Tree_Type
   is
      use Leander.Core.Trees;
      Pats : String_Vectors.Vector;
      Exps : Tree_Vectors.Vector;

      procedure Add_Binding
        (Name : String;
         Tree : Tree_Type);

      -----------------
      -- Add_Binding --
      -----------------

      procedure Add_Binding
        (Name : String;
         Tree : Tree_Type)
      is
      begin
         Pats.Append (Name);
         Exps.Append (Tree);
      end Add_Binding;

   begin
      Bindings.Scan_Local_Bindings (Add_Binding'Access);

      pragma Assert (Pats.Last_Index = Exps.Last_Index);

      declare
         Exp : Tree_Type := Expression;
      begin
         for I in reverse 1 .. Pats.Last_Index loop
            Exp :=
              Apply
                (Lambda
                   (Exps.Element (I).Head.Source,
                    Pats.Element (I)),
                 Exp);
         end loop;

         for I in 1 .. Exps.Last_Index loop
            Exp := Apply (Exp, Exps.Element (I));
         end loop;
         return Exp;
      end;

   end Let_Expression;

end Leander.Core.Lets;
