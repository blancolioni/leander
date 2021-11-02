with Leander.Core.Bindings;
with Leander.Logging;

package body Leander.Types.Class_Constraints is

   type Class_Record is
      record
         Head          : Leander.Types.Trees.Tree_Type;
         Type_Variable : Leander.Types.Trees.Tree_Type;
         Constraints   : Constraint_Lists.List;
         Methods       : Leander.Core.Bindings.Binding_List;
      end record;

   -----------------
   -- Add_Context --
   -----------------

   procedure Add_Context
     (Class   : in out Class_Constraint'Class;
      Context : Class_Constraint'Class)
   is
   begin
      Class.Class_Body.Constraints.Append (Context);
   end Add_Context;

   ----------------
   -- Add_Method --
   ----------------

   procedure Add_Method
     (Class     : in out Class_Constraint'Class;
      Name      : String;
      Signature : Leander.Types.Trees.Tree_Type;
      Default   : Leander.Core.Trees.Tree_Type)
   is
   begin
      Class.Class_Body.Methods.Insert (Name, Signature);
      if not Default.Is_Empty then
         Class.Class_Body.Methods.Insert
           (Name, Default,
            Bound_Name => Name & "-default");
      end if;
   end Add_Method;

   -------------
   -- Binding --
   -------------

   function Binding
     (Bindings : Class_Bindings;
      Name     : String)
      return Class_Constraint'Class
   is
   begin
      return Bindings.Map.Element (Name);
   end Binding;

   ------------
   -- Create --
   ------------

   procedure Create
     (Class : in out Class_Constraint'Class)
   is
   begin
      Class.Class_Body := new Class_Record;
   end Create;

   -----------------
   -- Has_Binding --
   -----------------

   function Has_Binding
     (Bindings : Class_Bindings;
      Name     : String)
      return Boolean
   is
   begin
      return Bindings.Map.Contains (Name);
   end Has_Binding;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Bindings : in out Class_Bindings;
      Name     : String;
      Binding  : Class_Constraint'Class)
   is
   begin
      Bindings.Map.Insert (Name, Binding);
   end Insert;

   ------------------
   -- Is_Subset_Of --
   ------------------

   overriding function Is_Subset_Of
     (Subset   : Class_Constraint;
      Superset : Class_Constraint)
      return Boolean
   is
   begin
      for Constraint of Subset.Class_Body.Constraints loop
         if Constraint = Type_Constraint'Class (Superset)
           or else Class_Constraint (Constraint).Is_Subset_Of (Superset)
         then
            return True;
         end if;
      end loop;
      return False;
   end Is_Subset_Of;

   ----------
   -- Name --
   ----------

   function Name
     (Class : Class_Constraint'Class)
      return String
   is
   begin
      return Class.Class_Body.Head.Left.Constructor_Name;
   end Name;

   ----------
   -- Scan --
   ----------

   procedure Scan
     (Bindings : Class_Bindings;
      Process  : not null access
        procedure (Binding : Class_Constraint'Class))
   is
   begin
      for Binding of Bindings.Map loop
         Process (Binding);
      end loop;
   end Scan;

   ------------------
   -- Scan_Methods --
   ------------------

   procedure Scan_Methods
     (Class   : Class_Constraint'Class;
      Process : not null access
        procedure (Name : String;
                   Signature : Leander.Types.Trees.Tree_Type;
                   Default   : Leander.Core.Trees.Tree_Type))
   is
      procedure Local_Process
        (Name      : String;
         Tree      : Leander.Core.Trees.Tree_Type;
         Signature : Leander.Types.Trees.Tree_Type);

      -------------------
      -- Local_Process --
      -------------------

      procedure Local_Process
        (Name      : String;
         Tree      : Leander.Core.Trees.Tree_Type;
         Signature : Leander.Types.Trees.Tree_Type)
      is
      begin
         Process (Name, Signature, Tree);
      end Local_Process;

   begin
      Class.Class_Body.Methods.Scan (Local_Process'Access);
   end Scan_Methods;

   --------------------
   -- Set_Constraint --
   --------------------

   procedure Set_Constraint
     (Class : in out Class_Constraint'Class;
      Name  : String;
      Tyvar : String)
   is
      Var_Node : Leander.Types.Type_Node :=
                   Leander.Types.Variable (Tyvar);
   begin
      Var_Node.Add_Constraint (Class);
      Class.Class_Body.Type_Variable := Leander.Types.Trees.Leaf (Var_Node);
      Class.Class_Body.Type_Variable.Set_Annotation
        (Leander.Kinds.Trees.Leaf
           (Leander.Kinds.Variable ('a')));
      Class.Class_Body.Head :=
        Leander.Types.Trees.Apply
          (Leander.Types.Constructor (Name),
           Class.Class_Body.Type_Variable);
      Leander.Logging.Log
        ("class: " & Leander.Types.Type_Constraint'Class (Class).Show
         & " " & Class.Type_Variable.Show_With_Annotations);

   end Set_Constraint;

   ----------
   -- Show --
   ----------

   overriding function Show
     (Constraint : Class_Constraint)
      return String
   is (Constraint.Class_Body.Head.Left.Show);

   -------------------
   -- Type_Variable --
   -------------------

   function Type_Variable
     (Class : Class_Constraint'Class)
      return Leander.Types.Trees.Tree_Type
   is (Class.Class_Body.Type_Variable);

end Leander.Types.Class_Constraints;
