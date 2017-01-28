with Leander.Logging;

package body Leander.Types.Class_Constraints.Compiler is

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Binding : Class_Constraint'Class;
      Env     : Leander.Environments.Environment;
      Machine : SK.Machine.SK_Machine)
   is
      pragma Unreferenced (Env);

      Method_Count : Natural := 0;
      Method_Index : Natural := 0;

      function Method_Name (Index : Positive) return String
      is ("m" & Integer'Image (-Index));

      procedure Compile_Method
        (Name : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type);

      procedure Record_Method
        (Name : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type);

      --------------------
      -- Compile_Method --
      --------------------

      procedure Compile_Method
        (Name      : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type)
      is
         pragma Unreferenced (Signature, Default);
      begin
         Method_Index := Method_Index + 1;
         Machine.Push ("vt");
         Machine.Push (Method_Name (Method_Index));
         Machine.Push ("vt");
         Machine.Apply;
         for I in reverse 1 .. Method_Count loop
            Machine.Lambda (Method_Name (I));
         end loop;
         Machine.Apply;
         Machine.Lambda ("vt");
         Leander.Logging.Log
           (Name & " = " & Machine.Show (Machine.Top));

         Machine.Bind (Name);
      end Compile_Method;

      -------------------
      -- Record_Method --
      -------------------

      procedure Record_Method
        (Name : String;
         Signature : Leander.Types.Trees.Tree_Type;
         Default   : Leander.Core.Trees.Tree_Type)
      is
         pragma Unreferenced (Name, Signature, Default);
      begin
         Method_Count := Method_Count + 1;
      end Record_Method;

   begin
      Binding.Scan_Methods (Record_Method'Access);
      Binding.Scan_Methods (Compile_Method'Access);
   end Compile;

end Leander.Types.Class_Constraints.Compiler;
