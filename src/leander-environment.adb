with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;
with Leander.Core.Binding_Groups.Inference;
with Leander.Core.Bindings;
with Leander.Core.Inference;
with Leander.Core.Type_Instances;
with Leander.Core.Types.Unification;
with Leander.Environment.Prelude;
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

   type Instance_Record is
      record
         Element    : Leander.Core.Type_Instances.Reference;
         Bindings   : Leander.Core.Binding_Groups.Reference;
         Dictionary : Leander.Calculus.Tree;
      end record;

   package Instance_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Instance_Record);

   package Instance_Maps is
     new Leander.Names.Maps (Instance_Lists.List, Instance_Lists."=");

   type Instance is new Abstraction with
      record
         Name      : Leander.Names.Leander_Name;
         Imports   : Import_Lists.List;
         Tycons    : Tycon_Maps.Map;
         Cons      : Con_Maps.Map;
         Bindings  : Leander.Core.Binding_Groups.Reference;
         Values    : Value_Map_Reference;
         Context   : Leander.Core.Inference.Inference_Context;
         Type_Env  : Leander.Core.Type_Env.Reference;
         Classes   : Type_Class_Maps.Map;
         Instances : Instance_Maps.Map;
      end record;

   overriding function Name (This : Instance) return String
   is (Leander.Names.To_String (This.Name));

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
     (This  : in out Instance;
      Class : Leander.Core.Type_Classes.Reference);

   overriding procedure Type_Instance
     (This          : in out Instance;
      Class_Id      : Leander.Core.Conid;
      Constraints   : Leander.Core.Predicates.Predicate_Array;
      Instance_Type : Leander.Core.Types.Reference;
      Bindings      : Leander.Core.Binding_Groups.Reference);

   overriding function Exists
     (This  : Instance;
      Name  : Leander.Names.Leander_Name;
      Class : Element_Class)
      return Boolean;

   overriding function Get_Class
     (This : Instance;
      Id   : Leander.Core.Conid)
      return Leander.Core.Type_Classes.Reference;

   overriding function Get_Instances
     (This : Instance;
      Id   : Leander.Core.Conid)
      return Leander.Core.Type_Instances.Reference_Array;

   overriding procedure Elaborate
     (This : in out Instance);

   overriding procedure Foreign_Import
     (This         : in out Instance;
      Name         : String;
      Foreign_Name : String;
      Signature    : Leander.Core.Types.Reference);

   overriding function Lookup
     (This : in out Instance;
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

   Local_Boot_Env : Reference := null;

   procedure Error
     (Message : String);

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

   ----------------------
   -- Boot_Environment --
   ----------------------

   function Boot_Environment return Reference is
   begin
      if Local_Boot_Env = null then
         Local_Boot_Env := Prelude.Create;
      end if;
      return Local_Boot_Env;
   end Boot_Environment;

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
      Leander.Core.Binding_Groups.Inference.Infer
        (Context, This.Bindings);

      This.Context := Context;
      This.Type_Env :=
        Context.Type_Env.Save (This.Type_Env, Context.Current_Substitution);

      if Context.OK then
         declare
            Ids : constant Core.Varid_Array := This.Bindings.Varids;
         begin
            for Id of Ids loop
               This.Bindings.Lookup (Leander.Names.Leander_Name (Id))
                 .Update_Type (Context);
            end loop;
         end;
      end if;

      for Class of This.Classes loop
         if This.Instances.Contains
           (Leander.Names.Leander_Name (Class.Id))
         then
            declare
               Methods : constant Core.Varid_Array :=
                           Class.Methods;
            begin
               for Inst of This.Instances.Element
                 (Leander.Names.Leander_Name (Class.Id))
               loop
                  declare
                     Inst_Context : Leander.Core.Inference.Inference_Context :=
                                      Leander.Core.Inference.Initial_Context
                                        (This.Type_Env);
                     Inst_Type    : constant Leander.Core.Types.Reference :=
                                      Inst.Element.Predicate.Get_Type;
                  begin
                     Leander.Core.Binding_Groups.Inference.Infer
                       (Inst_Context, Inst.Bindings);

                     if Inst_Type.Get_Tyvars'Length = 0 then
                        for P of Inst_Context.Current_Predicates loop
                           if P.Class_Name
                             = Inst.Element.Predicate.Class_Name
                             and then P.Get_Type.Get_Tyvars'Length > 0
                           then
                              Leander.Core.Types.Unification.Unify
                                (Inst_Context, P.Get_Type, Inst_Type);
                           end if;
                        end loop;
                     end if;

                     for Id of Methods loop
                        declare
                           use type Leander.Core.Bindings.Reference;
                           B : constant Leander.Core.Bindings.Reference :=
                                 Inst.Bindings.Lookup
                                   (Leander.Names.Leander_Name (Id));
                        begin
                           if B /= null then
                              B.Update_Type (Inst_Context);
                           end if;
                        end;
                     end loop;

                     declare
                        D : Leander.Calculus.Tree :=
                              Leander.Calculus.Symbol ("$class");
                     begin
                        for Id of Methods loop
                           declare
                              use type Leander.Core.Bindings.Reference;
                              Binding : constant Leander.Core.Bindings.Reference
                                := Inst.Bindings.Lookup
                                  (Leander.Names.Leander_Name (Id));
                           begin
                              if Binding = null then
                                 Error
                                   ("In instance "
                                    & Leander.Names.To_String
                                      (Leander.Names.Leander_Name (Class.Id))
                                    & " "
                                    & Inst.Element.Predicate.Show
                                    & ": "
                                    & "no class method "
                                    & Leander.Names.To_String
                                      (Leander.Names.Leander_Name (Id))
                                    & " found");
                              else
                                 Leander.Logging.Log
                                   ("INST", Binding.Show);
                                 declare
                                    Calc : constant Leander.Calculus.Tree :=
                                      Binding.To_Calculus
                                        (Inst_Context, This'Unchecked_Access);
                                 begin
                                    Leander.Logging.Log
                                      ("CALC", Leander.Calculus.To_String (Calc));
                                    D :=
                                      Leander.Calculus.Apply
                                        (D, Calc);
                                 end;
                              end if;
                           end;
                        end loop;
                        D := Leander.Calculus.Lambda ("$class", D);
                        declare
                           Dict_Name : constant String :=
                             "<" & Inst.Element.Predicate.Show & ">";
                        begin
                           if Leander.Calculus.Has_Reference
                             (D, Dict_Name)
                           then
                              D := Leander.Calculus.Apply
                                (Leander.Calculus.Symbol ("Y"),
                                 Leander.Calculus.Lambda (Dict_Name, D));
                           end if;
                           This.Values.Insert
                             (Leander.Names.To_Leander_Name (Dict_Name), D);
                        end;
                     end;
                  end;
               end loop;
            end;
         end if;
      end loop;
   end Elaborate;

   -----------
   -- Error --
   -----------

   procedure Error
     (Message : String)
   is
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Message);
   end Error;

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

      This.Type_Env := This.Type_Env.Compose (E.Type_Env);
   end Import;

   ------------
   -- Lookup --
   ------------

   overriding function Lookup
     (This : in out Instance;
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
               This.Context.Clear_Predicates;
               declare
                  Tree : Leander.Calculus.Tree :=
                            Binding.To_Calculus (This.Context, This'Access);
               begin
                  for P of This.Context.Current_Predicates loop
                     if P.Get_Type.all.Get_Tyvars'Length > 0
                     then
                        declare
                           Dict : constant String :=
                             "<" & P.Show & ">";
                        begin
                           Tree :=
                             Leander.Calculus.Lambda
                               (Dict, Tree);
                        end;
                     end if;
                  end loop;
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

   ---------------
   -- Get_Class --
   ---------------

   overriding function Get_Class
     (This : Instance;
      Id   : Leander.Core.Conid)
      return Leander.Core.Type_Classes.Reference
   is
   begin
      return This.Classes (Core.To_String (Id));
   end Get_Class;

   -------------------
   -- Get_Instances --
   -------------------

   overriding function Get_Instances
     (This : Instance;
      Id   : Leander.Core.Conid)
      return Leander.Core.Type_Instances.Reference_Array
   is
   begin
      if not This.Instances.Contains
           (Leander.Names.Leander_Name (Id))
      then
         return [];
      end if;
      declare
         Name renames Leander.Names.Leander_Name (Id);
         List : constant Instance_Lists.List := This.Instances.Element (Name);
         Last : Natural := 0;
      begin
         return R : Leander.Core.Type_Instances.Reference_Array
           (1 .. Natural (List.Length))
         do
            for Inst of List loop
               Last := Last + 1;
               R (Last) := Inst.Element;
            end loop;
            pragma Assert (Last = R'Last);
         end return;
      end;
   end Get_Instances;

   ----------------
   -- Type_Class --
   ----------------

   overriding procedure Type_Class
     (This  : in out Instance;
      Class : Leander.Core.Type_Classes.Reference)
   is
      Methods : constant Core.Varid_Array :=
                  Class.Methods;
   begin
      This.Classes.Insert (Core.To_String (Class.Id), Class);
      for I in Methods'Range loop
         This.Type_Env :=
           This.Type_Env.Compose
             (Methods (I),
              Class.Method_Scheme (Methods (I)));
         declare
            D : Leander.Calculus.Tree := Leander.Calculus.Symbol ("$inst");
            E : Leander.Calculus.Tree :=
                  Leander.Calculus.Symbol (I);
         begin
            for J in reverse Methods'Range loop
               E := Leander.Calculus.Lambda (J, E);
            end loop;
            D := Leander.Calculus.Apply (D, E);
            D := Leander.Calculus.Lambda ("$inst", D);
            This.Values.Insert
              (Leander.Names.Leander_Name (Methods (I)), D);
         end;
      end loop;
   end Type_Class;

   -------------------
   -- Type_Instance --
   -------------------

   overriding procedure Type_Instance
     (This          : in out Instance;
      Class_Id      : Leander.Core.Conid;
      Constraints   : Leander.Core.Predicates.Predicate_Array;
      Instance_Type : Leander.Core.Types.Reference;
      Bindings      : Leander.Core.Binding_Groups.Reference)
   is
      Inst       : constant Core.Type_Instances.Reference :=
                     Core.Type_Instances.Make_Instance
                       (Constraints,
                        Core.Predicates.Predicate (Class_Id, Instance_Type));
      Rec        : constant Instance_Record := Instance_Record'
        (Element    => Inst,
         Bindings   => Bindings,
         Dictionary => <>);
      Class_Name : constant Leander.Names.Leander_Name :=
                     Leander.Names.Leander_Name (Class_Id);
   begin
      if not This.Instances.Contains (Class_Name) then
         This.Instances.Insert (Class_Name, [Rec]);
      else
         declare
            List : Instance_Lists.List	:= This.Instances.Element (Class_Name);
         begin
            List.Append (Rec);
            This.Instances.Replace (Class_Name, List);
         end;
      end if;

   end Type_Instance;

end Leander.Environment;
