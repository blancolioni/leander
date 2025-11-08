with Ada.Text_IO;
with Leander.Core.Binding_Groups.Inference;
with Leander.Core.Bindings;
with Leander.Core.Inference;
with Leander.Names.Maps;
with WL.String_Maps;

package body Leander.Environment is

   package Tycon_Maps is
     new WL.String_Maps (Leander.Data_Types.Reference,
                         Leander.Data_Types."=");

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

   type Instance is new Abstraction with
      record
         Name     : Leander.Names.Leander_Name;
         Tycons   : Tycon_Maps.Map;
         Cons     : Con_Maps.Map;
         Bindings : Leander.Core.Binding_Groups.Reference;
         Values   : Value_Map_Reference;
         Context  : Leander.Core.Inference.Inference_Context;
         Type_Env : Leander.Core.Type_Env.Reference;
      end record;

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
            if Binding /= null then
               declare
                  Tree : constant Leander.Calculus.Tree :=
                           Binding.To_Calculus (This.Context, This'Access);
               begin
                  This.Values.Insert (Name, Tree);
                  return Tree;
               end;
            else
               raise Constraint_Error with
                 "undefined: " & Leander.Names.To_String (Name);
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

end Leander.Environment;
