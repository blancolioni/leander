with Ada.Containers.Doubly_Linked_Lists;

package body Leander.Core.Binding_Groups is

   package Container_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Bindings.Container_Reference, Bindings."=");

   type Instance is new Abstraction with
      record
         List : Container_Lists.List;
      end record;

   overriding function Show
     (This : Instance)
      return String;

   overriding function Implicit_Bindings
     (This : Instance)
      return Bindings.Container_Array;

   -------------------
   -- Binding_Group --
   -------------------

   function Binding_Group
     (Implicit_Bindings : Bindings.Container_Array)
      return Reference
   is
      List : Container_Lists.List;
   begin
      for Container of Implicit_Bindings loop
         List.Append (Container);
      end loop;
      return new Instance'(List => List);
   end Binding_Group;

   -----------------------
   -- Implicit_Bindings --
   -----------------------

   overriding function Implicit_Bindings
     (This : Instance)
      return Bindings.Container_Array
   is
      use Bindings, Container_Lists;

      function Inner
        (Position : Cursor)
         return Container_Array
      is (if not Has_Element (Position)
          then []
          else Element (Position) & Inner (Next (Position)));

   begin
      return Inner (This.List.First);
   end Implicit_Bindings;

   ----------
   -- Show --
   ----------

   overriding function Show
     (This : Instance)
      return String
   is
      function Show_Inner (Position : Container_Lists.Cursor) return String;

      ----------------
      -- Show_Inner --
      ----------------

      function Show_Inner (Position : Container_Lists.Cursor) return String is
      begin
         if not Container_Lists.Has_Element (Position) then
            return "";
         else
            declare
               Current : constant String :=
                           Container_Lists.Element (Position).Show;
               Next    : constant Container_Lists.Cursor :=
                           Container_Lists.Next (Position);
            begin
               if not Container_Lists.Has_Element (Next) then
                  return Current;
               else
                  return Current & "," & Show_Inner (Next);
               end if;
            end;
         end if;
      end Show_Inner;

   begin
      return "{" & Show_Inner (This.List.First) & "}";
   end Show;

end Leander.Core.Binding_Groups;
