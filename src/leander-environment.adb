with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Leander.Calculus;
with Leander.Core.Binding_Groups.Inference;
with Leander.Core.Inference;
with WL.String_Maps;

package body Leander.Environment is

   package Name_Id_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Names.Leander_Name, Leander.Names."=");

   type Data_Type_Record is
      record
         Tycon  : Leander.Core.Types.Reference;
         Cons   : Name_Id_Lists.List;
      end record;

   package Tycon_Maps is
     new WL.String_Maps (Data_Type_Record);

   type Con_Record is
      record
         Scheme : Leander.Core.Schemes.Reference;
         Defn   : Leander.Calculus.Tree;
      end record;

   package Con_Maps is
     new WL.String_Maps (Con_Record);

   type Instance is new Abstraction with
      record
         Name     : Leander.Names.Leander_Name;
         Tycons   : Tycon_Maps.Map;
         Cons     : Con_Maps.Map;
         Bindings : Leander.Core.Binding_Groups.Reference;
         Type_Env : Leander.Core.Type_Env.Reference;
      end record;

   overriding procedure Bindings
     (This   : in out Instance;
      Groups : Leander.Core.Binding_Groups.Reference);

   overriding function Constructor
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Schemes.Reference;

   overriding procedure Data_Type
     (This   : in out Instance;
      Tycon  : Leander.Core.Types.Reference;
      Kind   : Leander.Core.Kinds.Kind;
      Cons   : Leander.Core.Type_Env.Reference);

   overriding function Exists
     (This  : Instance;
      Name  : Leander.Names.Leander_Name;
      Class : Element_Class)
      return Boolean;

   overriding procedure Elaborate
     (This : in out Instance);

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

   ---------------
   -- Data_Type --
   ---------------

   overriding procedure Data_Type
     (This   : in out Instance;
      Tycon  : Leander.Core.Types.Reference;
      Kind   : Leander.Core.Kinds.Kind;
      Cons   : Leander.Core.Type_Env.Reference)
   is
      Name : constant String :=
               Leander.Core.To_String (Tycon.Constructor.Id);
      Ids  : constant Leander.Names.Name_Array := Cons.Ids;

      Data_Type_Rec : constant Data_Type_Record :=
                        Data_Type_Record'
                          (Tycon,
                           [for Id of Ids => Id]);
      Var_Ids       : constant Leander.Names.Name_Array :=
                        [for Id of Ids => Leander.Names.New_Name];

      function Con_Arg_Count
        (Scheme : Leander.Core.Schemes.Reference)
         return Natural;

      -------------------
      -- Con_Arg_Count --
      -------------------

      function Con_Arg_Count
        (Scheme : Leander.Core.Schemes.Reference)
         return Natural
      is
         use type Core.Types.Reference;
         T : Core.Types.Reference := Scheme.Fresh_Instance;
         Count : Natural := 0;
      begin
         while T.Is_Application
           and then T.Left.Is_Application
           and then T.Left.Left = Core.Types.T_Arrow
         loop
            Count := Count + 1;
            T := T.Right;
         end loop;
         return Count;
      end Con_Arg_Count;

      Con_Index : Natural := 0;
   begin
      This.Tycons.Insert
        (Key      => Name,
         New_Item => Data_Type_Rec);

      for Id of Cons.Ids loop
         Con_Index := Con_Index + 1;
         declare
            Scheme    : constant Leander.Core.Schemes.Reference :=
                          Cons.Element (Id);
            Arg_Count : constant Natural := Con_Arg_Count (Scheme);
            Pat_Ids   : constant Leander.Names.Name_Array (1 .. Arg_Count) :=
                          [others => Names.New_Name];
            E         : Calculus.Tree := Calculus.Symbol (Var_Ids (Con_Index));
         begin
            for Id of Pat_Ids loop
               E := Calculus.Apply (E, Calculus.Symbol (Id));
            end loop;
            for Id of reverse Var_Ids loop
               E := Calculus.Lambda (Id, E);
            end loop;
            for Id of reverse Pat_Ids loop
               E := Calculus.Lambda (Id, E);
            end loop;
            This.Cons.Insert
              (Leander.Names.To_String (Id),
               Con_Record'
                 (Scheme => Scheme,
                  Defn   => E));
            This.Type_Env := This.Type_Env.Compose (Core.Varid (Id), Scheme);
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

      This.Type_Env :=
        Context.Type_Env.Save (This.Type_Env, Context.Current_Substitution);

      --  This.Type_Env := Context.Type_Env.Compose (This.Type_Env);

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

   ---------------------
   -- New_Environment --
   ---------------------

   function New_Environment (Name : String) return Reference is
   begin
      return new Instance'
        (Name     => Leander.Names.To_Leander_Name (Name),
         Type_Env => Core.Type_Env.Empty,
         others   => <>);
   end New_Environment;

end Leander.Environment;
