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
     (This    : Tree;
      Machine : not null access Skit.Machine.Abstraction'Class)
   is
      package Variable_Index_Maps is
        new Leander.Names.Maps (Skit.Variable_Index, Skit."=");

      Index_Map : Variable_Index_Maps.Map;
      Next_Index : Skit.Variable_Index :=
                     Skit.Variable_Index'First;

      function To_Object
        (Name : Leander.Names.Leander_Name)
         return Skit.Object;

      procedure Go (This : Tree);

      --------
      -- Go --
      --------

      procedure Go (This : Tree) is
      begin
         case This.Class is
            when Number =>
               Machine.Push (Skit.To_Object (This.Value));
            when Reference =>
               Machine.Push (To_Object (This.Ref));
            when Apply =>
               Compile (This.L, Machine);
               Compile (This.R, Machine);
               Machine.Apply;
            when Lambda =>
               Machine.Push (Skit.Λ);
               Machine.Push (To_Object (This.X));
               Compile (This.E, Machine);
               Machine.Apply;
               Machine.Apply;
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

end Leander.Calculus;
