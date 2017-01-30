with Ada.Containers.Vectors;

with Leander.Errors;

with Leander.Core.Compiler;
with Leander.Core.Type_Inference;

with Leander.Logging;

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
          & Instance.Type_Tree.Head.Show
          & (if Index = 0 then "-vt"
             else Integer'Image (-Index)));

      procedure Add_Method
        (Name      : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type);

      function To_Instance_Type
        (Signature : Leander.Types.Trees.Tree_Type)
         return Leander.Types.Trees.Tree_Type;

      procedure Abstract_Type_Constraints
        (T : Leander.Types.Trees.Tree_Type);

      procedure Apply_Type_Constraints
        (T : Leander.Types.Trees.Tree_Type);

      -------------------------------
      -- Abstract_Type_Constraints --
      -------------------------------

      procedure Abstract_Type_Constraints
        (T : Leander.Types.Trees.Tree_Type)
      is
      begin
         for Constraint of T.Head.Constraints loop
            declare
               Name : constant String :=
                        Constraint.Show & "-" & T.Head.Show & "-vt";
            begin
               Machine.Lambda (Name);
            end;
         end loop;
      end Abstract_Type_Constraints;

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
                     else Default.Clean_Copy);
      begin
         Method.Set_Annotation (To_Instance_Type (Signature));
         Leander.Logging.Log (Name & " :: " & Method.Annotation.Show);
         Leander.Core.Type_Inference.Annotate (Method, Env);
         Methods.Append (Method);
      end Add_Method;

      ----------------------------
      -- Apply_Type_Constraints --
      ----------------------------

      procedure Apply_Type_Constraints
        (T : Leander.Types.Trees.Tree_Type)
      is
      begin
         for Constraint of T.Head.Constraints loop
            declare
               Name : constant String :=
                        Constraint.Show & "-" & T.Head.Show & "-vt";
            begin
               Machine.Push (Name);
               Machine.Apply;
            end;
         end loop;
      end Apply_Type_Constraints;

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
            return Instance.Type_Tree;
         elsif Signature.Is_Application then
            return To_Instance_Type (Signature.Left).Apply
              (To_Instance_Type (Signature.Right));
         else
            return Signature;
         end if;
      end To_Instance_Type;

   begin

      Leander.Logging.Log
        ("Compiling " & Instance.Class_Name & "-"
         & Instance.Type_Tree.Show);

      Class.Scan_Methods (Add_Method'Access);

      if Leander.Errors.Has_Errors then
         return;
      end if;

      for I in 1 .. Methods.Last_Index loop
         Leander.Core.Compiler.Compile_Instance_Method
           (Env           => Env,
            Name          => Method_Name (I),
            Instance_Name => Method_Name (0),
            Tree          => Methods (I),
            Machine       => Machine);
      end loop;

      Machine.Push ("-op-");

      for I in 1 .. Methods.Last_Index loop
         Machine.Push (Method_Name (I));
         Instance.Scan_Constraints (Apply_Type_Constraints'Access);
         Machine.Apply;
      end loop;
      Machine.Lambda ("-op-");

      declare
      begin
         Instance.Scan_Constraints (Abstract_Type_Constraints'Access);
      end;

      Leander.Logging.Log
        (Method_Name (0) & " = " & Machine.Show (Machine.Top));

      Machine.Bind (Method_Name (0));

   end Compile;

end Leander.Types.Instances.Compiler;
