with Leander.Core.Bindings;
with Leander.Syntax.Expressions;

package body Leander.Syntax.Bindings is

   -----------------
   -- Add_Binding --
   -----------------

   procedure Add_Binding
     (This : in out Instance;
      Pat  : not null access constant Patterns.Instance'Class;
      Expr : not null access constant Expressions.Instance'Class)
   is
      use type Leander.Core.Name_Id;
      Head : constant Patterns.Reference := Pat.Head;
   begin
      if not Pat.Is_Variable then
         raise Constraint_Error with
         Leander.Source.Show (Pat.Location)
           & "only variable binding patterns supported";
      end if;

      if This.Bindings.Is_Empty
        or else This.Bindings.Last_Element.Name
          /= Core.Id (Head.Variable_Name)
      then
         This.Bindings.Append
           (Name_Binding'
              (Core.Id (Head.Variable_Name),
               []));
      end if;

      This.Bindings (This.Bindings.Last).Equations.Append
        (Binding_Record'
           (Patterns.Reference (Pat),
            Expression_Reference (Expr)));
   end Add_Binding;

   -----------
   -- Empty --
   -----------

   function Empty return Reference is
   begin
      return new Instance;
   end Empty;

   -------------
   -- To_Core --
   -------------

   function To_Core
     (This : Instance)
      return Leander.Core.Binding_Groups.Reference
   is
      Container : constant Leander.Core.Bindings.Container_Reference :=
                    Leander.Core.Bindings.Container
                      ([for B of This.Bindings =>
                          Core.Bindings.Bind
                            (B.Name,
                             B.Equations.First_Element.Expr.To_Core)]);
   begin
      return Leander.Core.Binding_Groups.Binding_Group
        ([Container]);
   end To_Core;

end Leander.Syntax.Bindings;
