with Ada.Containers.Doubly_Linked_Lists;

package body Leander.Core.Assumptions is

   type Entry_Record is
      record
         Id      : Name_Id;
         Binding : Leander.Core.Types.Reference;
      end record;

   package List_Of_Entries is
      new Ada.Containers.Doubly_Linked_Lists (Entry_Record);

   type Instance is new Abstraction with
      record
         List : List_Of_Entries.List;
      end record;

   overriding function Find
     (This : Instance;
      Id   : Name_Id)
      return Maybe_Types.Maybe;

   overriding function Append
     (This    : Instance;
      Id      : Name_Id;
      Binding : Types.Reference)
      return Reference;

   overriding function Append
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference;

   overriding function Prepend
     (This    : Instance;
      Id      : Name_Id;
      Binding : Types.Reference)
      return Reference;

   overriding function Prepend
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference;

   Local_Empty_Assumptions : aliased constant Instance :=
                               (List => []);

   function Empty return Reference
   is (Local_Empty_Assumptions'Access);

   ------------
   -- Append --
   ------------

   overriding function Append
     (This    : Instance;
      Id      : Name_Id;
      Binding : Types.Reference)
      return Reference
   is
      Result : Instance := This;
   begin
      Result.List.Append (Entry_Record'(Id, Binding));
      return new Instance'(Result);
   end Append;

   ------------
   -- Append --
   ------------

   overriding function Append
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference
   is
      Result : Instance := This;
   begin
      for Element of Instance (That.all).List loop
         Result.List.Append (Element);
      end loop;
      return new Instance'(Result);
   end Append;

   ----------------
   -- Assumption --
   ----------------

   function Assumption
     (Id : Name_Id;
      T  : Types.Reference)
      return Reference
   is
   begin
      return Empty.Append (Id, T);
   end Assumption;

   ----------
   -- Find --
   ----------

   overriding function Find
     (This : Instance;
      Id   : Name_Id)
      return Maybe_Types.Maybe
   is
   begin
      for Element of This.List loop
         if Element.Id = Id then
            return Maybe_Types.Just (Element.Binding);
         end if;
      end loop;
      return Maybe_Types.Nothing;
   end Find;

   -------------
   -- Prepend --
   -------------

   overriding function Prepend
     (This    : Instance;
      Id      : Name_Id;
      Binding : Types.Reference)
      return Reference
   is
      Result : Instance := This;
   begin
      Result.List.Prepend (Entry_Record'(Id, Binding));
      return new Instance'(Result);
   end Prepend;

   -------------
   -- Prepend --
   -------------

   overriding function Prepend
     (This    : Instance;
      That    : not null access constant Abstraction'Class)
      return Reference
   is
      Result : Instance := Instance (That.all);
   begin
      for Element of This.List loop
         Result.List.Append (Element);
      end loop;
      return new Instance'(Result);
   end Prepend;
end Leander.Core.Assumptions;
