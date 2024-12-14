package body Leander.Core.Expressions is

   type Instance_Class is
     (Variable, Constructor, Literal, Application, Lambda, Let);

   subtype Parent is Abstraction;

   type Instance (Class : Instance_Class) is new Parent with
      record
         case Class is
            when Variable =>
               Var_Id      : Name_Id;
            when Constructor =>
               Con_Id      : Name_Id;
               Con_Type    : Types.Reference;
            when Literal =>
               Literal     : Literals.Reference;
            when Application =>
               Left, Right : Reference;
            when Lambda =>
               Lambda_Var  : Name_Id;
               Lambda_Body : Reference;
            when Let =>
               Let_Binding : Bindings.Reference;
               Let_Expr    : Reference;
         end case;
      end record;

   overriding function Show
     (This : Instance)
      return String;

   overriding procedure Visit
     (This    : not null access constant Instance;
      Visitor : in out Expression_Visitor'class);

   function Allocate
     (This : Instance'Class)
      return Reference;

   --------------
   -- Allocate --
   --------------

   function Allocate
     (This : Instance'Class)
      return Reference
   is
   begin
      return new Instance'Class'(This);
   end Allocate;

   -----------
   -- Apply --
   -----------

   function Apply (Left, Right : Reference) return Reference is
   begin
      return Allocate (Instance'(Application, Left, Right));
   end Apply;

   -----------------
   -- Constructor --
   -----------------

   function Constructor
     (Id    : Name_Id;
      CType : Types.Reference)
      return Reference
   is
   begin
      return Allocate (Instance'(Constructor, Id, CType));
   end Constructor;

   ------------
   -- Lambda --
   ------------

   function Lambda (Id         : Name_Id;
                    Expression : Reference)
                    return Reference
   is
   begin
      return Allocate (Instance'(Lambda, Id, Expression));
   end Lambda;

   ---------
   -- Let --
   ---------

   function Let (Binding     : Bindings.Reference;
                 Expression  : Reference)
                 return Reference
   is
   begin
      return Allocate (Instance'(Let, Binding, Expression));
   end Let;

   -------------
   -- Literal --
   -------------

   function Literal (Lit : Literals.Reference) return Reference is
   begin
      return Allocate (Instance'(Literal, Lit));
   end Literal;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
   begin
      case This.Class is
         when Variable =>
            return Show (This.Var_Id);
         when Constructor =>
            return Show (This.Con_Id);
         when Literal =>
            return This.Literal.Show;
         when Application =>
            if Instance (This.Right.all).Class in
              Application | Lambda | Let
            then
               return This.Left.Show & " ("
                 & This.Right.Show & ")";
            else
               return This.Left.Show & " " & This.Right.Show;
            end if;
         when Lambda =>
            return "\" & Show (This.Lambda_Var) & " -> "
              & This.Lambda_Body.Show;
         when Let =>
            return "let " & This.Let_Binding.Show
              & " in " & This.Let_Expr.Show;
      end case;
   end Show;

   --------------
   -- Variable --
   --------------

   function Variable (Id : Name_Id) return Reference is
   begin
      return Allocate (Instance'(Variable, Id));
   end Variable;

   -----------
   -- Visit --
   -----------

   overriding procedure Visit
     (This    : not null access constant Instance;
      Visitor : in out Expression_Visitor'class)
   is
   begin
      case This.Class is
         when Variable =>
            Visitor.Variable (This.Var_Id);
         when Constructor =>
            Visitor.Constructor (This.Con_Id, This.Con_Type);
         when Literal =>
            Visitor.Literal (This.Literal);
         when Application =>
            Visitor.Application (This.Left, This.Right);
         when Lambda =>
            Visitor.Lambda (This.Lambda_Var, This.Lambda_Body);
         when Let =>
            Visitor.Let (This.Let_Binding, This.Let_Expr);
      end case;
   end Visit;

end Leander.Core.Expressions;
