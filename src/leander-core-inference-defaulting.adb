with Leander.Core.Type_Classes;
with Leander.Core.Types.Unification;

package body Leander.Core.Inference.Defaulting is

   function Is_Numeric (Class : String) return Boolean
   is (Class = "Num" or else Class = "Real" or else Class = "Integral"
       or else Class = "Fractional" or else Class = "Floating"
       or else Class = "RealFrac" or else Class = "RealFloat");

   function Is_Standard (Class : String) return Boolean
   is (Is_Numeric (Class)
       or else Class = "Eq" or else Class = "Ord" or else Class = "Show"
       or else Class = "Read" or else Class = "Enum"
       or else Class = "Bounded" or else Class = "Ix");

   --  The types tried, in order: default (Integer, Double) in the Report,
   --  but Int is the only number type there is (#86, #85).
   function Defaults return Leander.Core.Types.Type_Array
   is ([1 => Leander.Core.Types.T_Int]);

   procedure Default_Reduced
     (Context    : in out Inference_Context'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Keep       : Leander.Core.Tyvars.Tyvar_Array);
   --  As Default_Ambiguous, for predicates already in head-normal form.

   -----------------------
   -- Default_Ambiguous --
   -----------------------

   procedure Default_Ambiguous
     (Context    : in out Inference_Context'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Keep       : Leander.Core.Tyvars.Tyvar_Array)
   is
      use type Leander.Core.Predicates.Predicate_Array;

      function Reduce (Index : Positive)
                       return Leander.Core.Predicates.Predicate_Array;
      --  Predicates (Index ..), each in head-normal form where an instance
      --  makes it so: the Report defaults after context reduction, so
      --  that Eq [a] counts as Eq a.  One no instance reduces is kept as
      --  it is, and so stops its variables being defaulted.

      ------------
      -- Reduce --
      ------------

      function Reduce (Index : Positive)
                       return Leander.Core.Predicates.Predicate_Array
      is
      begin
         if Index > Predicates'Last then
            return [];
         end if;

         declare
            P : constant Leander.Core.Predicates.Instance :=
                  Leander.Core.Predicates.Predicate
                    (Predicates (Index).Class_Id,
                     Leander.Core.Types.Reference
                       (Predicates (Index).Get_Type.Apply
                          (Context.Current_Substitution)));
            Head : Leander.Core.Predicates.Predicate_Array (1 .. 1) := [P];
         begin
            if Context.Class_Environment = null then
               return Head & Reduce (Index + 1);
            end if;

            begin
               return Context.Class_Environment.To_Head_Normal_Form (P)
                 & Reduce (Index + 1);
            exception
               when Constraint_Error =>
                  Head (1) := P;
                  return Head & Reduce (Index + 1);
            end;
         end;
      end Reduce;

   begin
      if Predicates'Length > 0 then
         Default_Reduced (Context, Reduce (Predicates'First), Keep);
      end if;
   end Default_Ambiguous;

   ---------------------
   -- Default_Reduced --
   ---------------------

   procedure Default_Reduced
     (Context    : in out Inference_Context'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array;
      Keep       : Leander.Core.Tyvars.Tyvar_Array)
   is
      Done : Leander.Core.Tyvars.Tyvar_Array (1 .. Predicates'Length);
      Last : Natural := 0;

      function Type_Of
        (P : Leander.Core.Predicates.Instance)
         return Leander.Core.Types.Reference
      is (Leander.Core.Types.Reference
            (P.Get_Type.Apply (Context.Current_Substitution)));

      function Instance_Exists
        (Class : Conid;
         T     : Leander.Core.Types.Reference)
         return Boolean;
      --  Whether Class T holds.  Without the classes to ask -- in a
      --  class's or an instance's own context -- assume it does for the
      --  classes Int is known to have instances of.

      ---------------------
      -- Instance_Exists --
      ---------------------

      function Instance_Exists
        (Class : Conid;
         T     : Leander.Core.Types.Reference)
         return Boolean
      is
      begin
         if Context.Class_Environment = null then
            return Class /= To_Conid ("Fractional")
              and then Class /= To_Conid ("Floating")
              and then Class /= To_Conid ("RealFrac")
              and then Class /= To_Conid ("RealFloat");
         end if;
         return Context.Class_Environment.Entails
           ([], Leander.Core.Predicates.Predicate (Class, T));
      end Instance_Exists;

   begin
      for P of Predicates loop
         declare
            T : constant Leander.Core.Types.Reference := Type_Of (P);
         begin
            if T.Is_Variable
              and then Leander.Core.Tyvars.Intersection
                ([T.Variable], Keep)'Length = 0
              and then Leander.Core.Tyvars.Intersection
                ([T.Variable], Done (1 .. Last))'Length = 0
            then
               Last := Last + 1;
               Done (Last) := T.Variable;

               declare
                  V        : constant Leander.Core.Tyvars.Instance :=
                               T.Variable;
                  Simple   : Boolean := True;
                  Numeric  : Boolean := False;
                  Standard : Boolean := True;
               begin
                  --  Every predicate that mentions V.
                  for Q of Predicates loop
                     declare
                        QT : constant Leander.Core.Types.Reference :=
                               Type_Of (Q);
                     begin
                        if Leander.Core.Tyvars.Intersection
                          ([V], QT.Get_Tyvars)'Length > 0
                        then
                           if not QT.Is_Variable then
                              Simple := False;
                           end if;
                           Numeric := Numeric or else Is_Numeric (Q.Class_Name);
                           Standard :=
                             Standard and then Is_Standard (Q.Class_Name);
                        end if;
                     end;
                  end loop;

                  if Simple and then Numeric and then Standard then
                     for D of Defaults loop
                        if (for all Q of Predicates =>
                              Leander.Core.Tyvars.Intersection
                                ([V], Type_Of (Q).Get_Tyvars)'Length = 0
                              or else Instance_Exists (Q.Class_Id, D))
                        then
                           Leander.Core.Types.Unification.Unify
                             (Context, Leander.Core.Types.TVar (V), D);
                           exit;
                        end if;
                     end loop;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Default_Reduced;

   ---------------------
   -- Check_Instances --
   ---------------------

   procedure Check_Instances
     (Context    : in out Inference_Context'Class;
      Predicates : Leander.Core.Predicates.Predicate_Array)
   is
   begin
      if Context.Class_Environment = null then
         return;
      end if;

      for P of Predicates loop
         declare
            Q : constant Leander.Core.Predicates.Instance :=
                  Leander.Core.Predicates.Predicate
                    (P.Class_Id,
                     Leander.Core.Types.Reference
                       (P.Get_Type.Apply (Context.Current_Substitution)));
         begin
            if Q.Get_Type.Get_Tyvars'Length = 0
              and then not Context.Class_Environment.Entails ([], Q)
            then
               Context.Error ("no instance for " & Q.Show);
               return;
            end if;
         end;
      end loop;
   end Check_Instances;

end Leander.Core.Inference.Defaulting;
