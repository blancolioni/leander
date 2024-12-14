with Leander.Core.Expressions;

package body Leander.Core.Bindings is

   type Instance is new Abstraction with
      record
         Id    : Name_Id;
         Value : Expressions.Reference;
      end record;

   overriding function Show
     (This : Instance)
      return String
   is (Show (This.Id) & "=" & This.Value.Show);

   overriding function Id (This : Instance) return Name_Id
   is (This.Id);

   overriding function Binding
     (This : Instance)
      return not null access constant Expressions.Abstraction'Class
   is (This.Value);

   ----------
   -- Bind --
   ----------

   function Bind
     (Id    : Name_Id;
      Value : not null access constant Expressions.Abstraction'Class)
      return Reference
   is
   begin
      return new Instance'(Id => Id, Value => Expressions.Reference (Value));
   end Bind;

end Leander.Core.Bindings;
