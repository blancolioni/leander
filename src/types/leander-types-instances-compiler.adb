with Ada.Containers.Vectors;
with Ada.Text_IO;

with SK.Machine.Assembler;

with Leander.Core.Compiler;
with Leander.Core.Type_Inference;

package body Leander.Types.Instances.Compiler is

   package Core_Tree_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Core.Trees.Tree_Type, Leander.Core.Trees."=");

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Env      : Leander.Environments.Environment;
      Instance : Type_Instance'Class;
      Machine  : SK.Machine.SK_Machine)
   is
      Class : constant Class_Constraints.Class_Constraint'Class :=
                Env.Class_Binding (Instance.Class_Name);
      Methods : Core_Tree_Vectors.Vector;

      function Method_Name (Index : Natural) return String
      is (Instance.Class_Name
          & "-"
          & Instance.Type_Tree.Show
          & (if Index = 0 then "-vt"
             else Integer'Image (-Index)));

      procedure Add_Method
        (Name      : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type);

      function To_Instance_Type
        (Signature : Leander.Types.Trees.Tree_Type)
         return Leander.Types.Trees.Tree_Type;

      ----------------
      -- Add_Method --
      ----------------

      procedure Add_Method
        (Name      : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type)
      is
         Method : constant Leander.Core.Trees.Tree_Type :=
                    (if Instance.Has_Method_Body (Name)
                     then Instance.Method_Body (Name)
                     else Default);
      begin
         Method.Set_Annotation (To_Instance_Type (Signature));
         Leander.Core.Type_Inference.Annotate (Method, Env);
         Methods.Append (Method);
      end Add_Method;

      ----------------------
      -- To_Instance_Type --
      ----------------------

      function To_Instance_Type
        (Signature : Leander.Types.Trees.Tree_Type)
         return Leander.Types.Trees.Tree_Type
      is
      begin
         if Signature.Is_Leaf
           and then Signature.Is_Variable
           and then Signature.Variable_Name = Class.Type_Variable.Show
         then
            return Instance.Type_Tree.First_Leaf;
         elsif Signature.Is_Application then
            return To_Instance_Type (Signature.Left).Apply
              (To_Instance_Type (Signature.Right));
         else
            return Signature;
         end if;
      end To_Instance_Type;

   begin

      Ada.Text_IO.Put_Line
        ("Compiling " & Instance.Class_Name & "-"
         & Instance.Type_Tree.Show);

      Class.Scan_Methods (Add_Method'Access);

      for I in 1 .. Methods.Last_Index loop
         Leander.Core.Compiler.Compile
           (Env     => Env,
            Name    => Method_Name (I),
            Tree    => Methods (I),
            Machine => Machine);
      end loop;

      SK.Machine.Assembler.Push
        (Machine, "-op-");
      for I in 1 .. Methods.Last_Index loop
         SK.Machine.Assembler.Push (Machine, Method_Name (I));
         SK.Machine.Assembler.Apply (Machine);
      end loop;
      SK.Machine.Assembler.Lambda (Machine, "-op-");

      Ada.Text_IO.Put_Line
        (Method_Name (0) & " = " & SK.Machine.Show_Stack_Top (Machine));

      SK.Machine.Bind (Machine, Method_Name (0));

   end Compile;

end Leander.Types.Instances.Compiler;
