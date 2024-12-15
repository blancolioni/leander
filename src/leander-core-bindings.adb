with Ada.Containers.Doubly_Linked_Lists;
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

   package Reference_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Reference);

   type Container_Instance is new Container_Abstraction with
      record
         Elements : Reference_Lists.List;
      end record;

   overriding function Show
     (This : Container_Instance)
      return String;

   overriding function To_Array
     (This : Container_Instance)
      return Reference_Array;

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

   ---------------
   -- Container --
   ---------------

   function Container
     (Bindings : Reference_Array)
      return Container_Reference
   is
      List : Reference_Lists.List;
   begin
      for Element of Bindings loop
         List.Append (Element);
      end loop;
      return new Container_Instance'(Elements => List);
   end Container;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Container_Instance)
      return String
   is
      function Show_Inner (Position : Reference_Lists.Cursor) return String;

      ----------------
      -- Show_Inner --
      ----------------

      function Show_Inner (Position : Reference_Lists.Cursor) return String is
      begin
         if not Reference_Lists.Has_Element (Position) then
            return "";
         else
            declare
               Current : constant String :=
                           Reference_Lists.Element (Position).Show;
               Next    : constant Reference_Lists.Cursor :=
                           Reference_Lists.Next (Position);
            begin
               if not Reference_Lists.Has_Element (Next) then
                  return Current;
               else
                  return Current & "," & Show_Inner (Next);
               end if;
            end;
         end if;
      end Show_Inner;

   begin
      return "{" & Show_Inner (This.Elements.First) & "}";
   end Show;

   --------------
   -- To_Array --
   --------------

   overriding function To_Array
     (This : Container_Instance)
      return Reference_Array
   is
      use Reference_Lists;

      function Inner
        (Position : Cursor)
         return Reference_Array
      is (if not Has_Element (Position)
          then []
          else Element (Position) & Inner (Next (Position)));

   begin
      return Inner (This.Elements.First);
   end To_Array;

end Leander.Core.Bindings;
