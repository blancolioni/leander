with Ada.Text_IO;

with Leander.Types.Kind_Inference;
with Leander.Types.Class_Constraints.Compiler;
with Leander.Types.Instances.Compiler;

with Leander.Core.Compiler;
with Leander.Core.Type_Inference;

package body Leander.Environments is

   ------------------------
   -- Add_Type_Assertion --
   ------------------------

   procedure Add_Type_Assertion
     (Env       : Environment;
      Name      : String;
      Assertion : Leander.Types.Type_Assertion'Class)
   is
   begin
      Env.Local.Types.Add_Assertion (Name, Assertion);
   end Add_Type_Assertion;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Env  : Environment'Class)
   is
   begin
      Leander.Types.Kind_Inference.Infer_Kinds
        (Environment (Env));
      Leander.Core.Type_Inference.Infer_Types
        (Environment (Env));
   end Annotate;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (Env  : Leander.Environments.Environment;
      Machine : SK.Machine.SK_Machine)
   is

      procedure Compile_Binding
        (Name : String;
         Tree : Leander.Core.Trees.Tree_Type);

      procedure Compile_Class
        (Class : Leander.Types.Class_Constraints.Class_Constraint'Class);

      procedure Compile_Instances
        (Name    : String;
         Binding : Leander.Types.Bindings.Type_Binding'Class);

      ---------------------
      -- Compile_Binding --
      ---------------------

      procedure Compile_Binding
        (Name : String;
         Tree : Leander.Core.Trees.Tree_Type)
      is
      begin
         if not Tree.Is_Empty then
            Leander.Core.Compiler.Compile
              (Env, Name, Tree, Machine);
         end if;
      end Compile_Binding;

      -------------------
      -- Compile_Class --
      -------------------

      procedure Compile_Class
        (Class : Leander.Types.Class_Constraints.Class_Constraint'Class)
      is
      begin
         Leander.Types.Class_Constraints.Compiler.Compile
           (Class, Env, Machine);
      end Compile_Class;

      -----------------------
      -- Compile_Instances --
      -----------------------

      procedure Compile_Instances
        (Name    : String;
         Binding : Leander.Types.Bindings.Type_Binding'Class)
      is

         pragma Unreferenced (Name);

         procedure Compile_Instance_Assertion
           (Assertion : Leander.Types.Type_Assertion'Class);

         --------------------------------
         -- Compile_Instance_Assertion --
         --------------------------------

         procedure Compile_Instance_Assertion
           (Assertion : Leander.Types.Type_Assertion'Class)
         is
         begin
            Leander.Types.Instances.Compiler.Compile
              (Env, Leander.Types.Instances.Type_Instance (Assertion),
               Machine);
         end Compile_Instance_Assertion;

      begin
         Ada.Text_IO.Put_Line
           ("compiling instances for " & Binding.Type_Pattern.Show);

         Binding.Type_Pattern.Head.Scan_Assertions
           (Compile_Instance_Assertion'Access);
      end Compile_Instances;

   begin
      Env.Local.Classes.Scan (Compile_Class'Access);
      Env.Local.Types.Scan_Bindings (Compile_Instances'Access);
      Env.Local.Values.Scan (Compile_Binding'Access);
   end Compile;

   ------------
   -- Create --
   ------------

   procedure Create
     (Env  : in out Environment'Class;
      Name : String)
   is
   begin
      Env.Local := new Environment_Record;
      Env.Local.Name := Ada.Strings.Unbounded.To_Unbounded_String (Name);
   end Create;

   ----------------------------------
   -- Create_Temporary_Environment --
   ----------------------------------

   procedure Create_Temporary_Environment
     (Env    : in out Environment'Class;
      Parent : Environment'Class;
      Name   : String)
   is
   begin
      Env.Create (Name);
      Env.Global := Parent.Local;
   end Create_Temporary_Environment;

   -----------------------
   -- Declare_Data_Type --
   -----------------------

   procedure Declare_Data_Type
     (Env       : in out Environment;
      Name      : String;
      Data_Type : Leander.Types.Trees.Tree_Type)
   is
   begin
      Env.Local.Types.Declare_Data_Type (Name, Data_Type);
   end Declare_Data_Type;

   ----------------------
   -- Declare_New_Type --
   ----------------------

   procedure Declare_New_Type
     (Env       : in out Environment;
      Name      : String;
      New_Type  : Leander.Types.Trees.Tree_Type)
   is
   begin
      Env.Local.Types.Declare_New_Type (Name, New_Type);
   end Declare_New_Type;

   ----------------------------
   -- Declare_Primitive_Type --
   ----------------------------

   procedure Declare_Primitive_Type
     (Env            : in out Environment;
      Name           : String;
      Primitive_Type : Leander.Types.Trees.Tree_Type)
   is
   begin
      Env.Local.Types.Declare_Primitive_Type (Name, Primitive_Type);
   end Declare_Primitive_Type;

   ------------------
   -- Import_Names --
   ------------------

   procedure Import_Names
     (To   : in out Environment'Class;
      From : Environment'Class)
   is
      pragma Unreferenced (From, To);
   begin
      null;
   end Import_Names;

   --------------------------
   -- Insert_Class_Binding --
   --------------------------

   procedure Insert_Class_Binding
     (Env   : Environment;
      Name  : String;
      Class : Leander.Types.Class_Constraints.Class_Constraint'Class)
   is
   begin
      Env.Local.Classes.Insert (Name, Class);
   end Insert_Class_Binding;

   ------------------------
   -- Insert_Constructor --
   ------------------------

   procedure Insert_Constructor
     (Env       : in out Environment;
      Type_Name : String;
      Name      : String;
      Con_Type  : Leander.Types.Trees.Tree_Type)
   is
      Binding : constant Leander.Types.Bindings.Constructor_Binding'Class :=
                  Env.Local.Types.Add_Constructor
                    (Type_Name, Name, Con_Type);
   begin
      Env.Local.Constructors.Insert (Name, Binding);
   end Insert_Constructor;

   ---------------------------
   -- Insert_Foreign_Import --
   ---------------------------

   procedure Insert_Foreign_Import
     (Env          : in out Environment;
      Name         : String;
      Foreign_Name : String;
      Signature    : Leander.Types.Trees.Tree_Type)
   is
      pragma Unreferenced (Foreign_Name);
   begin
      Env.Local.Values.Insert (Name, Signature);
      Ada.Text_IO.Put_Line (Name & " :: " & Signature.Show);
   end Insert_Foreign_Import;

   ----------------------
   -- Insert_Signature --
   ----------------------

   procedure Insert_Signature
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Types.Trees.Tree_Type)
   is
   begin
      Env.Local.Values.Insert (Name, Value);
      Ada.Text_IO.Put_Line (Name & " :: " & Value.Show);
   end Insert_Signature;

   --------------------------
   -- Insert_Type_Variable --
   --------------------------

   procedure Insert_Type_Variable
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Types.Trees.Tree_Type)
   is
   begin
      Env.Local.Types.Insert_Type_Variable (Name, Value);
   end Insert_Type_Variable;

   ------------------
   -- Insert_Value --
   ------------------

   procedure Insert_Value
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Core.Trees.Tree_Type)
   is
   begin
      Ada.Text_IO.Put_Line (Name & " = " & Value.Show);
      Env.Local.Values.Insert (Name, Value);
   end Insert_Value;

   -------------------------
   -- Scan_Local_Bindings --
   -------------------------

   procedure Scan_Local_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type))
   is
   begin
      Env.Local.Values.Scan (Process);
   end Scan_Local_Bindings;

   -------------------------
   -- Scan_Local_Bindings --
   -------------------------

   procedure Scan_Local_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Core.Trees.Tree_Type;
                   Signature : Leander.Types.Trees.Tree_Type))
   is
   begin
      Env.Local.Values.Scan (Process);
   end Scan_Local_Bindings;

   -------------------------------
   -- Scan_Local_Class_Bindings --
   -------------------------------

   procedure Scan_Local_Class_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Binding : Types.Class_Constraints.Class_Constraint'Class))
   is
   begin
      Env.Local.Classes.Scan (Process);
   end Scan_Local_Class_Bindings;

   ------------------------------
   -- Scan_Local_Type_Bindings --
   ------------------------------

   procedure Scan_Local_Type_Bindings
     (Env     : Environment'Class;
      Process : not null access
        procedure (Name : String;
                   Tree : Leander.Types.Bindings.Type_Binding'Class))
   is
   begin
      Env.Local.Types.Scan_Bindings (Process);
   end Scan_Local_Type_Bindings;

end Leander.Environments;
