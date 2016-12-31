with Ada.Text_IO;

with Leander.Core.Compiler;
with Leander.Core.Type_Inference;

package body Leander.Environments is

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Env  : Environment'Class)
   is
   begin
      Leander.Core.Type_Inference.Infer_Types
        (Bindings  => Env.Local.Values,
         Types     => Env.Local.Types,
         Cons      => Env.Local.Constructors,
         Env       => Environment (Env));
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

      ---------------------
      -- Compile_Binding --
      ---------------------

      procedure Compile_Binding
        (Name : String;
         Tree : Leander.Core.Trees.Tree_Type)
      is
      begin
         Leander.Core.Compiler.Compile
           (Env, Name, Tree, Machine);
      end Compile_Binding;

   begin
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

   ------------------
   -- Insert_Value --
   ------------------

   procedure Insert_Value
     (Env   : in out Environment;
      Name  : String;
      Value : Leander.Core.Trees.Tree_Type)
   is
   begin
      Env.Local.Values.Insert (Name, Value);
      Ada.Text_IO.Put_Line (Name & " = " & Value.Show);
   end Insert_Value;

end Leander.Environments;
