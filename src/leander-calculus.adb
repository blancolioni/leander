with Ada.Unchecked_Deallocation;

package body Leander.Calculus is

   type Node_Class is
     (Integer_Constant, Long_Float_Constant,
      Reference, Apply, Lambda);

   type Node_Record (Class : Node_Class) is
      record
         case Class is
            when Integer_Constant =>
               Integer_Value    : Integer;
            when Long_Float_Constant =>
               Long_Float_Value : Long_Float;
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

   function Compile
     (This     : Tree)
      return Skit.Terms.Term
   is
   begin
      case This.Class is
         when Integer_Constant =>
            return Skit.Terms.Const (This.Integer_Value);
         when Long_Float_Constant =>
            return Skit.Terms.Const (This.Long_Float_Value);
         when Reference =>
            return Skit.Terms.Symbol (Leander.Names.To_String (This.Ref));
         when Apply =>
            return Skit.Terms.Apply
              (Compile (This.L), Compile (This.R));
         when Lambda =>
            return Skit.Terms.Lambda
              (Leander.Names.To_String (This.X), Compile (This.E));
      end case;
   end Compile;

   -------------
   -- Dispose --
   -------------

   procedure Dispose (This : in out Tree) is

   begin
      case This.Class is
         when Integer_Constant =>
            null;
         when Long_Float_Constant =>
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

   -------------------
   -- Has_Reference --
   -------------------

   function Has_Reference
     (This : Tree;
      Name : String)
      return Boolean
   is
      use type Leander.Names.Leander_Name;
      N : constant Leander.Names.Leander_Name :=
            Leander.Names.To_Leander_Name (Name);
   begin
      case This.Class is
         when Integer_Constant =>
            return False;
         when Long_Float_Constant =>
            return False;
         when Reference =>
            return This.Ref = N;
         when Apply =>
            return Has_Reference (This.L, Name)
              or else Has_Reference (This.R, Name);
         when Lambda =>
            return This.X /= N
              and then Has_Reference (This.E, Name);
      end case;
   end Has_Reference;

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
   -- Lambda --
   ------------

   function Lambda
     (Index : Natural;
      Expr  : Tree)
      return Tree
   is
      Img : constant String := Index'Image;
   begin
      return Lambda (Img (2 .. Img'Last), Expr);
   end Lambda;

   ------------
   -- Number --
   ------------

   function Number
     (Value : Integer)
      return Tree
   is
   begin
      return new Node_Record'(Integer_Constant, Value);
   end Number;

   ------------
   -- Number --
   ------------
   function Number
     (Value : Long_Float)
      return Tree
   is
   begin
      return new Node_Record'(Long_Float_Constant, Value);
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

   ------------
   -- Symbol --
   ------------

   function Symbol
     (Index : Natural)
      return Tree
   is
      Img : constant String := Index'Image;
   begin
      return Symbol (Img (2 .. Img'Last));
   end Symbol;

   ---------------
   -- To_String --
   ---------------

   function To_String (This : Tree) return String is
   begin
      case This.Class is
         when Integer_Constant =>
            return This.Integer_Value'Image;
         when Long_Float_Constant =>
            return This.Long_Float_Value'Image;
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
