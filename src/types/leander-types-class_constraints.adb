package body Leander.Types.Class_Constraints is

   -----------------
   -- Add_Context --
   -----------------

   procedure Add_Context
     (Class   : in out Class_Constraint'Class;
      Context : Class_Constraint'Class)
   is
   begin
      Class.Constraints.Append (Context);
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
      Class.Methods.Insert (Name, Signature);
      if not Default.Is_Empty then
         Class.Methods.Insert (Name, Default);
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
     (Class : in out Class_Constraint'Class;
      Name  : String;
      Tyvar : String)
   is
      Kind : constant Leander.Kinds.Trees.Tree_Type :=
               Leander.Kinds.Trees.Leaf
                 (Leander.Kinds.Variable ('a'));
   begin
      Class.Head :=
        Leander.Types.Trees.Apply
          (Leander.Types.Constructor (Name, Kind),
           Leander.Types.Variable (Tyvar, Kind));

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

end Leander.Types.Class_Constraints;
