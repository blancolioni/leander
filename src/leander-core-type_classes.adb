with Leander.Core.Substitutions;

package body Leander.Core.Type_Classes is

   function Match_Predicate
     (Instance_Predicate : Leander.Core.Predicates.Instance;
      Check_Predicate    : Leander.Core.Predicates.Instance;
      Success            : out Boolean)
      return Leander.Core.Substitutions.Instance;

   function To_Head_Normal_Form
     (This       : Class_Environment'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Success    : out Boolean)
      return Leander.Core.Predicates.Predicate_Array;

   function Simplify
     (This       : Class_Environment'Class;
      Predicates : Leander.core.Predicates.Predicate_Array)
      return Leander.core.Predicates.Predicate_Array;

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

   -----------------
   -- By_Instance --
   -----------------

   function By_Instance
     (This      : Class_Environment'Class;
      Predicate : Leander.Core.Predicates.Instance;
      Success   : out Boolean)
      return Leander.Core.Predicates.Predicate_Array
   is
      Instances : constant Leander.Core.Type_Instances.Reference_Array :=
                     This.All_Instances (Predicate.Class_Id);
   begin
      for Inst of Instances loop
         declare
            Subst   : constant Leander.Core.Substitutions.Instance :=
               Match_Predicate (Inst.Predicate, Predicate, Success);
         begin
            if Success then
               return Inst.Qualifier.Apply (subst).Predicates;
            end if;
         end;
      end loop;
      Success := False;
      return [];
   end By_Instance;

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
         for C of This.Super_Classes (P.Class_Id) loop
            if C = Check.Class_Id then
               return True;
            end if;
         end loop;
      end loop;

      declare
         Success : Boolean;
         Ps      : constant Leander.Core.Predicates.Predicate_Array :=
                     This.By_Instance (Check, Success);
      begin
         if Success then
            for P of Ps loop
               if not This.Entails (Current, P) then
                  return False;
               end if;
            end loop;
            return True;
         end if;
      end;

      return False;
   end Entails;

   ---------------------
   -- Match_Predicate --
   ---------------------

   function Match_Predicate
     (Instance_Predicate : Leander.Core.Predicates.Instance;
      Check_Predicate    : Leander.Core.Predicates.Instance;
      Success            : out Boolean)
      return Leander.Core.Substitutions.Instance
   is
   begin
      if Instance_Predicate.Class_Id = Check_Predicate.Class_Id then
         return Instance_Predicate.Get_Type.Match
                  (Check_Predicate.Get_Type, Success);
      else
         Success := False;
         return Leander.Core.Substitutions.Empty;
      end if;
   end Match_Predicate;

   ------------
   -- Reduce --
   ------------

   function Reduce
     (This       : Class_Environment'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Success    : out Boolean)
      return Leander.Core.Predicates.Predicate_Array
   is
      Qs : constant Leander.Core.Predicates.Predicate_Array :=
        This.To_Head_Normal_Form (Predicates, Success);
   begin
      if Success then
         return This.Simplify (Qs);
      end if;
      return [];
   end Reduce;

   --------------
   -- Simplify --
   --------------

   function Simplify
     (This       : Class_Environment'Class;
      Predicates : Leander.core.Predicates.Predicate_Array)
      return Leander.core.Predicates.Predicate_Array
   is
      use Leander.Core.Predicates;
      function Go (Acc : Predicate_Array; Index : Positive) return Predicate_Array;

      function Go (Acc : Predicate_Array; Index : Positive) return Predicate_Array is
      begin
         if Index > Predicates'Last then
            return Acc;
         else
            declare
               P : constant Core.Predicates.Instance := Predicates (Index);
            begin
               if This.Entails (Acc & Predicates (Index + 1 .. Predicates'Last), P) then
                  return Go (Acc, Index + 1);
               else
                  return Go (Acc & P, Index + 1);
               end if;
            end;
         end if;
      end Go;

   begin
      return Go ([], Predicates'First);
   end Simplify;

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

   -------------------------
   -- To_Head_Normal_Form --
   -------------------------

   function To_Head_Normal_Form
     (This      : Class_Environment'Class;
      Predicate : Leander.core.Predicates.Instance)
      return Leander.core.Predicates.Predicate_Array
   is
   begin
      if Predicate.Get_Type.Head_Normal_Form then
         return [Predicate];
      else
         declare
            use type Leander.Core.Predicates.Predicate_Array;
            Success : Boolean;
            Ps      : constant Leander.Core.Predicates.Predicate_Array :=
                        This.By_Instance (Predicate, Success);
         begin
            if not Success then
               raise Constraint_Error
                 with "No instance found for "
                      & Predicate.Show;
            else
               declare
                  function HNF
                  (Idx : Positive)
                  return Leander.Core.Predicates.Predicate_Array
                  is (if Idx <= Ps'Last
                     then This.To_Head_Normal_Form (Ps (Idx))
                     & HNF (Idx + 1)
                     else []);
               begin
                  return HNF (Ps'First);
               end;
            end if;
         end;
      end if;
   end To_Head_Normal_Form;

   function To_Head_Normal_Form
     (This       : Class_Environment'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Success    : out Boolean)
      return Leander.Core.Predicates.Predicate_Array
   is
      use type Leander.Core.Predicates.Predicate_Array;
      function HNF
        (Idx : Positive)
         return Leander.Core.Predicates.Predicate_Array
      is (if Idx <= Predicates'Last
         then This.To_Head_Normal_Form (Predicates (Idx))
         & HNF (Idx + 1)
         else []);
   begin
      Success := True;
      return HNF (Predicates'First);
   end To_Head_Normal_Form;

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
