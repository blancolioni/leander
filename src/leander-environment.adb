with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Leander.Core.Binding_Groups.Inference;
with Leander.Core.Bindings;
with Leander.Core.Inference;
with Leander.Logging;
with Leander.Names.Maps;
with WL.String_Maps;

package body Leander.Environment is

   package Tycon_Maps is
     new WL.String_Maps (Leander.Data_Types.Reference,
                         Leander.Data_Types."=");

   package Type_Class_Maps is
     new WL.String_Maps (Leander.Core.Type_Classes.Reference,
                         Leander.Core.Type_Classes."=");

   type Con_Record is
      record
         Scheme : Leander.Core.Schemes.Reference;
         Defn   : Leander.Calculus.Tree;
         DT     : Leander.Data_Types.Reference;
      end record;

   package Con_Maps is
     new WL.String_Maps (Con_Record);

   package Value_Maps is
     new Leander.Names.Maps
       (Leander.Calculus.Tree, Leander.Calculus."=");

   type Value_Map_Reference is access Value_Maps.Map;

   package Import_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   type Instance is new Abstraction with
      record
         Name     : Leander.Names.Leander_Name;
         Imports  : Import_Lists.List;
         Tycons   : Tycon_Maps.Map;
         Cons     : Con_Maps.Map;
         Bindings : Leander.Core.Binding_Groups.Reference;
         Values   : Value_Map_Reference;
         Context  : Leander.Core.Inference.Inference_Context;
         Type_Env : Leander.Core.Type_Env.Reference;
         Classes  : Type_Class_Maps.Map;
      end record;

   overriding procedure Import
     (This : in out Instance;
      Env  : not null access Abstraction'Class);

   overriding procedure Bindings
     (This   : in out Instance;
      Groups : Leander.Core.Binding_Groups.Reference);

   overriding function Constructor
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Schemes.Reference;

   overriding function Constructor
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Calculus.Tree;

   overriding function Con_Data_Type
     (This : Instance;
      Id   : Leander.Core.Conid)
      return Leander.Data_Types.Reference
   is (This.Cons.Element (Core.To_String (Id)).DT);

   overriding function Data_Type
     (This : Instance;
      Id   : Leander.Core.Conid)
      return Leander.Data_Types.Reference
   is (This.Tycons.Element (Core.To_String (Id)));

   overriding procedure Data_Type
     (This   : in out Instance;
      DT     : Leander.Data_Types.Reference);

   overriding procedure Type_Class
     (This : in out Instance;
      Class : Leander.Core.Type_Classes.Reference);

   overriding function Exists
     (This  : Instance;
      Name  : Leander.Names.Leander_Name;
      Class : Element_Class)
      return Boolean;

   overriding procedure Elaborate
     (This : in out Instance);

   overriding procedure Foreign_Import
     (This         : in out Instance;
      Name         : String;
      Foreign_Name : String;
      Signature    : Leander.Core.Types.Reference);

   overriding function Lookup
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Calculus.Tree;

   overriding function Contains
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Boolean;

   overriding function Type_Env
     (This : Instance)
      return Leander.Core.Type_Env.Reference
   is (This.Type_Env);

   --------------
   -- Bindings --
   --------------

   overriding procedure Bindings
     (This   : in out Instance;
      Groups : Leander.Core.Binding_Groups.Reference)
   is
   begin
      This.Bindings := Groups;
   end Bindings;

   -----------------
   -- Constructor --
   -----------------

   overriding function Constructor
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Schemes.Reference
   is
   begin
      return This.Cons.Element (Leander.Names.To_String (Name)).Scheme;
   end Constructor;

   -----------------
   -- Constructor --
   -----------------

   overriding function Constructor
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Calculus.Tree
   is
   begin
      return This.Cons.Element (Leander.Names.To_String (Name)).Defn;
   end Constructor;

   --------------
   -- Contains --
   --------------

   overriding function Contains
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Boolean
   is
      use type Leander.Core.Bindings.Reference;
   begin
      return This.Values.Contains (Name)
        or else This.Bindings.Lookup (Name) /= null
        or else (for some Import of This.Imports => Import.Contains (Name));
   end Contains;

   ---------------
   -- Data_Type --
   ---------------

   overriding procedure Data_Type
     (This   : in out Instance;
      DT     : Leander.Data_Types.Reference)
   is
   begin
      This.Tycons.Insert (Core.To_String (DT.Id), DT);
      for I in 1 .. DT.Constructor_Count loop
         declare
            Id : constant Leander.Core.Conid :=
                   DT.Constructor_Name (I);
         begin
            This.Values.Insert
              (Leander.Names.Leander_Name (Id), DT.Constructor_Calculus (I));
            This.Cons.Insert
              (Core.To_String (Id),
               Con_Record'
                 (DT.Constructor_Type (I),
                  DT.Constructor_Calculus (I),
                  DT));
            This.Type_Env :=
              This.Type_Env.Compose
                (Core.Varid (Id),
                 DT.Constructor_Type (I));
         end;
      end loop;
   end Data_Type;

   ---------------
   -- Elaborate --
   ---------------

   overriding procedure Elaborate
     (This : in out Instance)
   is
      Context : Leander.Core.Inference.Inference_Context :=
                  Leander.Core.Inference.Initial_Context (This.Type_Env);
   begin
      Leander.Logging.Log ("ELAB", Leander.Names.To_String (This.Name));
      Leander.Core.Binding_Groups.Inference.Infer
        (Context, This.Bindings);

      This.Context := Context;
      This.Type_Env :=
        Context.Type_Env.Save (This.Type_Env, Context.Current_Substitution);

      if not Context.OK then
         Ada.Text_IO.Put_Line
           (Context.Error_Message);
      end if;
   end Elaborate;

   ------------
   -- Exists --
   ------------

   overriding function Exists
     (This  : Instance;
      Name  : Leander.Names.Leander_Name;
      Class : Element_Class)
      return Boolean
   is
      N : constant String := Leander.Names.To_String (Name);
   begin
      case Class is
         when Constructor =>
            return This.Cons.Contains (N);
         when Type_Constructor =>
            return This.Tycons.Contains (N);
         when Variable_Binding =>
            return False;
         when Class_Binding =>
            return This.Classes.Contains (N);
      end case;
   end Exists;

   --------------------
   -- Foreign_Import --
   --------------------

   overriding procedure Foreign_Import
     (This         : in out Instance;
      Name         : String;
      Foreign_Name : String;
      Signature    : Leander.Core.Types.Reference)
   is
   begin
      This.Values.Insert
        (Leander.Names.To_Leander_Name (Name),
         Leander.Calculus.Symbol (Foreign_Name));
      This.Type_Env :=
        This.Type_Env.Compose
          (Core.To_Varid (Name),
           Leander.Core.Schemes.To_Scheme (Signature));
   end Foreign_Import;

   ------------
   -- Import --
   ------------

   overriding procedure Import
     (This : in out Instance;
      Env  : not null access Abstraction'Class)
   is
      E : Instance'Class renames Instance'Class (Env.all);
   begin
      Leander.Logging.Log
        ("IMPORT", Leander.Names.To_String (E.Name));
      This.Imports.Append (Reference (Env));
      for Position in E.Tycons.Iterate loop
         if not This.Tycons.Contains (Tycon_Maps.Key (Position)) then
            This.Tycons.Insert (Tycon_Maps.Key (Position),
                                Tycon_Maps.Element (Position));
         end if;
      end loop;
      for Position in E.Cons.Iterate loop
         if not This.Cons.Contains (Con_Maps.Key (Position)) then
            This.Cons.Insert (Con_Maps.Key (Position),
                                Con_Maps.Element (Position));
         end if;
      end loop;

      Leander.Logging.Log
        ("IMPORT", E.Type_Env.Show);
      This.Type_Env := This.Type_Env.Compose (E.Type_Env);
   end Import;

   ------------
   -- Lookup --
   ------------

   overriding function Lookup
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Calculus.Tree
   is
   begin
      if This.Values.Contains (Name) then
         return This.Values.Element (Name);
      else
         declare
            use type Leander.Core.Bindings.Reference;
            Binding : constant Leander.Core.Bindings.Reference :=
                     This.Bindings.Lookup (Name);
         begin
            if Binding = null then
               for Import of This.Imports loop
                  if Import.Contains (Name) then
                     return Import.Lookup (Name);
                  end if;
               end loop;
               raise Constraint_Error with
                 "undefined: " & Leander.Names.To_String (Name);
            else
               declare
                  Tree : constant Leander.Calculus.Tree :=
                           Binding.To_Calculus (This.Context, This'Access);
               begin
                  This.Values.Insert (Name, Tree);
                  return Tree;
               end;
            end if;
         end;
      end if;
   end Lookup;

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment (Name : String) return Reference is
   begin
      return new Instance'
        (Name     => Leander.Names.To_Leander_Name (Name),
         Type_Env => Core.Type_Env.Empty,
         Values   => new Value_Maps.Map,
         others   => <>);
   end New_Environment;

   ----------------
   -- Type_Class --
   ----------------

   overriding procedure Type_Class
     (This : in out Instance;
      Class : Leander.Core.Type_Classes.Reference)
   is
   begin
      This.Classes.Insert (Core.To_String (Class.Id), Class);
   end Type_Class;

end Leander.Environment;
