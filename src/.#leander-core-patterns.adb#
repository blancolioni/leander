with Leander.Allocator;
with Leander.Core.Binding_Groups;

package body Leander.Core.Patterns is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("expressions", Instance, Variable_Reference);

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance)
      return Reference
   is
   begin
      return Reference (Allocator.Allocate (This));
   end Allocate;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (Id   : Conid;
      Args : Conargs)
      return Reference
   is
   begin
      return Allocate ((PCon, Args'Length, Core.Typeable.New_Id, Id, Args));
   end Constructor;

   -------------
   -- Dispose --
   -------------

   overriding procedure Dispose (This : in out Instance) is
   begin
      null;
   end Dispose;

   -------------------
   -- Has_Reference --
   -------------------

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean
   is
   begin
      case This.Tag is
         when PVar =>
            return This.Var_Id = To;
         when PCon =>
            return (for some Id of This.Con_Args => Id = To);
         when PLit =>
            return False;
      end case;
   end Has_Reference;

   -------------
   -- Literal --
   -------------

   function Literal (Lit : Literals.Instance) return Reference is
   begin
      return Allocate ((PLit, 0, Core.Typeable.New_Id, Lit));
   end Literal;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
      function To_String (Args : Conargs) return String;

      ---------------
      -- To_String --
      ---------------

      function To_String (Args : Conargs) return String is
         function Show_Arg (Index : Positive) return String
         is (if Index <= Args'Last
             then " " & To_String (Args (Index)) & Show_Arg (Index + 1)
             else "");
      begin
         return Show_Arg (Args'First);
      end To_String;

   begin
      case This.Tag is
         when PVar =>
            return To_String (This.Var_Id);
         when PCon =>
            return To_String (This.Con_Id)
              & To_String (This.Con_Args);
         when PLit =>
            return This.Literal.Show;
      end case;
   end Show;

   --------------
   -- Traverse --
   --------------

   overriding procedure Traverse
     (This : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Traverseable.Abstraction'Class))
   is
   begin
      Process (This);
      case This.Tag is
         when PVar =>
            null;
         when PCon =>
            null;
         when PLit =>
            null;
      end case;
   end Traverse;

   --------------
   -- Variable --
   --------------

   function Variable (Id : Varid) return Reference is
   begin
      return Allocate (Instance'(PVar, 0, Core.Typeable.New_Id, Id));
   end Variable;

end Leander.Core.Patterns;
