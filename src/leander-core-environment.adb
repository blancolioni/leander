package body Leander.Core.Environment is

   type Nullable_Reference is access constant Abstraction'Class;

   type Entry_Record is
      record
         Binding : Bindings.Reference;
         Next    : Nullable_Reference;
      end record;

   type Entry_Reference is access Entry_Record;

   type Instance is new Abstraction with
      record
         First, Last : Entry_Reference;
      end record;

   overriding function Find
     (This : Instance;
      Id   : Name_Id)
      return Maybe_Binding.Maybe;

   overriding function Append
     (This    : Instance;
      Binding : Bindings.Reference)
      return Reference;

   Local_Empty_Environment : aliased constant Instance :=
                               (null, null);

   function Empty_Environment return Reference
   is (Local_Empty_Environment'Access);

   ----------
   -- Find --
   ----------

   overriding function Find
     (This : Instance;
      Id   : Name_Id)
      return Maybe_Binding.Maybe
   is
      It : Entry_Reference := This.First;
   begin
      while It /= null loop
         if It.Binding.Id = Id then
            return Maybe_Binding.Just (It.Binding);
         end if;
         It := It.Next;
      end loop;
      return Maybe_Binding.Nothing;
   end Find;

end Leander.Core.Environment;
