with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Unchecked_Deallocation;
with Leander.Names.Maps;

package body Leander.Calculus is

   type Node_Class is
     (Number, Reference, Apply, Lambda);

   type Node_Record (Class : Node_Class) is
      record
         case Class is
            when Number =>
               Value : Integer;
            when Reference =>
               Ref   : Leander.Names.Leander_Name;
            when Apply =>
               L, R  : Tree;
            when Lambda =>
               X     : Leander.Names.Leander_Name;
               E     : Tree;
         end case;
      end record;

   procedure Free is
     new Ada.Unchecked_Deallocation (Node_Record, Tree);

   -----------
   -- Apply --
   -----------

   function Apply
     (Left, Right : Tree)
      return Tree
   is
   begin
      return new Node_Record'(Apply, Left, Right);
   end Apply;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (This     : Tree;
      Env      : not null access constant Calculus_Environment'Class;
      Skit_Env : Skit.Environment.Reference)
   is
      package Variable_Index_Maps is
        new Leander.Names.Maps (Skit.Variable_Index, Skit."=");

      type Lambda_Binding_Record is
         record
            Name : Leander.Names.Leander_Name;
            Obj  : Skit.Object;
         end record;

      package Lambda_Binding_Lists is
        new Ada.Containers.Doubly_Linked_Lists (Lambda_Binding_Record);

      Index_Map : Variable_Index_Maps.Map;
      Next_Index : Skit.Variable_Index :=
                     Skit.Variable_Index'First;
      Lambda_Bindings : Lambda_Binding_Lists.List;

      function To_Object
        (Name : Leander.Names.Leander_Name)
         return Skit.Object;

      procedure Go
        (This : Tree);

      --------
      -- Go --
      --------

      procedure Go
        (This : Tree)
      is
         use type Leander.Names.Leander_Name;
         use type Skit.Object;
      begin
         case This.Class is
            when Number =>
               Skit_Env.Machine.Push (Skit.To_Object (This.Value));
            when Reference =>
               for Binding of reverse Lambda_Bindings loop
                  if Binding.Name = This.Ref then
                     Skit_Env.Machine.Push (Binding.Obj);
                     return;
                  end if;
               end loop;

               declare
                  Binding : constant Skit.Object :=
                              Skit_Env.Lookup
                                (Leander.Names.To_String (This.Ref));
               begin
                  if Binding = Skit.Undefined then
                     declare
                        T : constant Tree := Env.Lookup (This.Ref);
                     begin
                        if T = null then
                           raise Program_Error with
                             "undefined: "
                             & Leander.Names.To_String (This.Ref);
                        end if;
                        Compile (T, Env, Skit_Env);
                        Skit_Env.Bind (Leander.Names.To_String (This.Ref),
                                       Skit_Env.Machine.Top);
                     end;
                  else
                     Skit_Env.Machine.Push (Binding);
                  end if;
               end;

            when Apply =>
               Go (This.L);
               Go (This.R);
               Skit_Env.Machine.Apply;
            when Lambda =>
               Skit_Env.Machine.Push (Skit.Lambda);
               declare
                  X : constant Skit.Object := To_Object (This.X);
               begin
                  Skit_Env.Machine.Push (X);
                  Lambda_Bindings.Append
                    (Lambda_Binding_Record'
                       (This.X, X));
                  Go (This.E);
                  Lambda_Bindings.Delete_Last;
               end;
               Skit_Env.Machine.Apply;
               Skit_Env.Machine.Apply;
         end case;
      end Go;

      ---------------
      -- To_Object --
      ---------------

      function To_Object
        (Name : Leander.Names.Leander_Name)
         return Skit.Object
      is
         use type Skit.Variable_Index;
         Index : Skit.Variable_Index;
      begin
         if not Index_Map.Contains (Name) then
            Index_Map.Insert (Name, Next_Index);
            Index := Next_Index;
            Next_Index := @ + 1;
         else
            Index := Index_Map.Element (Name);
         end if;
         return Skit.To_Variable_Object (Index);
      end To_Object;

   begin
      Go (This);
   end Compile;

   -------------
   -- Dispose --
   -------------

   procedure Dispose (This : in out Tree) is

   begin
      case This.Class is
         when Number =>
            null;
         when Reference =>
            null;
         when Apply =>
            Dispose (This.L);
            Dispose (This.R);
         when Lambda =>
            Dispose (This.E);
      end case;
      Free (This);
   end Dispose;

   ------------
   -- Lambda --
   ------------

   function Lambda
     (Name : String;
      Expr : Tree)
      return Tree
   is
   begin
      return Lambda (Leander.Names.To_Leander_Name (Name), Expr);
   end Lambda;

   ------------
   -- Lambda --
   ------------

   function Lambda
     (Name : Leander.Names.Leander_Name;
      Expr : Tree)
      return Tree
   is
   begin
      return new Node_Record'(Lambda, Name, Expr);
   end Lambda;

   ------------
   -- Number --
   ------------

   function Number
     (Value : Integer)
      return Tree
   is
   begin
      return new Node_Record'(Number, Value);
   end Number;

   ---------------
   -- Reference --
   ---------------

   function Symbol
     (Name : String)
      return Tree
   is
      use Leander.Names;
   begin
      return Symbol (To_Leander_Name (Name));
   end Symbol;

   ------------
   -- Symbol --
   ------------

   function Symbol
     (Name : Leander.Names.leander_Name)
      return Tree
   is
   begin
      return new Node_Record'(Reference, Name);
   end Symbol;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Tree) return String is
   begin
      case This.Class is
         when Number =>
            return This.Value'Image;
         when Reference =>
            return Leander.Names.To_String (This.Ref);
         when Apply =>
            return "(" & To_String (This.L)
              & " " & To_String (This.R) & ")";
         when Lambda =>
            return "\" & Leander.Names.To_String (This.X)
              & "." & To_String (This.E);
      end case;
   end To_String;

end Leander.Calculus;
