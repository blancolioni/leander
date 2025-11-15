with Ada.Text_IO;

package body Leander.Core.Type_Classes is

   -------------------
   -- All_Instances --
   -------------------

   function All_Instances
     (This      : Class_Environment'Class;
      Class_Id  : Conid)
      return Leander.Core.Type_Instances.Reference_Array
   is
   begin
      return This.Get_Instances (Class_Id);
   end All_Instances;

   -------------
   -- Entails --
   -------------

   function Entails
     (This    : Class_Environment'Class;
      Current : Core.Predicates.Predicate_Array;
      Check   : Core.Predicates.Instance)
      return Boolean
   is
   begin
      for P of Current loop
         Ada.Text_IO.Put_Line
           ("entails: " & P.Show & " <> " & Check.Show);
         for C of This.Super_Classes (Check.Class_Id) loop
            Ada.Text_IO.Put_Line ("super: " & To_String (C));
            if C = P.Class_Id then
               return True;
            end if;
         end loop;
      end loop;

      for C of This.Super_Classes (Check.Class_Id) loop
         for Inst of This.All_Instances (C) loop
            if Inst.Predicate.Class_Id = C
              and then Inst.Predicate.Get_Type.Equivalent (Check.Get_Type)
            then
               return True;
            end if;
         end loop;
      end loop;

      return False;
   end Entails;

   -------------------
   -- Super_Classes --
   -------------------

   function Super_Classes
     (This : Instance'Class)
      return Leander.Core.Conid_Array
   is
   begin
      return Cons : Leander.Core.Conid_Array (This.Predicates'Range) do
         for I in This.Predicates'Range loop
            Cons (I) := This.Predicates (I).Class_Id;
         end loop;
      end return;
   end Super_Classes;

   -------------------
   -- Super_Classes --
   -------------------

   function Super_Classes
     (This : Class_Environment'Class;
      Id   : Conid)
      return Leander.Core.Conid_Array
   is
      Cs : constant Conid_Array := This.Get_Class (Id).Super_Classes;

      function Super (Index : Positive) return Conid_Array
      is (if Index in Cs'Range
          then This.Super_Classes (Cs (Index)) & Super (Index + 1)
          else []);

   begin
      return Id & Super (Cs'First);
   end Super_Classes;

   ----------------
   -- Type_Class --
   ----------------

   function Type_Class
     (Class_Name    : Conid;
      Variable_Name : Varid;
      Predicates    : Leander.Core.Predicates.Predicate_Array;
      Bindings      : Leander.Core.Binding_Groups.Reference)
      return Reference
   is
   begin
      return new Instance'
        (Predicate_Count  => Predicates'Length,
         Class_Id         => Class_Name,
         Var_Id           => Variable_Name,
         Predicates       => Predicates,
         Bindings         => Bindings);
   end Type_Class;

end Leander.Core.Type_Classes;
