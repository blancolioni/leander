with Ada.Containers.Vectors;

with Leander.Allocator;
with Leander.Core.Alts.Compiler;
with Leander.Core.Expressions;
with Leander.Core.Patterns;

package body Leander.Core.Bindings is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("bindings", Instance, Variable_Reference);

   package Predicate_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Core.Predicates.Instance,
        Leander.Core.Predicates."=");

   package Dictionary_Vectors is
     new Ada.Containers.Vectors
       (Positive, Predicate_Vectors.Vector, Predicate_Vectors."=");

   --  One entry per binding, indexed by Dict_Id.  Holding the predicates
   --  here keeps Instance free of a variable-length component and lets
   --  Set_Dictionaries work through an access-constant Reference.
   Dictionaries_Table : Dictionary_Vectors.Vector;

   function Next_Dict_Id return Positive;

   ------------------
   -- Dictionaries --
   ------------------

   function Dictionaries
     (This : Instance'Class)
      return Leander.Core.Predicates.Predicate_Array
   is
      Ps : Predicate_Vectors.Vector renames
             Dictionaries_Table (This.Dict_Id);
   begin
      return [for P of Ps => P];
   end Dictionaries;

   ------------------
   -- Next_Dict_Id --
   ------------------

   function Next_Dict_Id return Positive is
   begin
      Dictionaries_Table.Append (Predicate_Vectors.Empty_Vector);
      return Dictionaries_Table.Last_Index;
   end Next_Dict_Id;

   ----------------------
   -- Set_Dictionaries --
   ----------------------

   procedure Set_Dictionaries
     (This : Instance'Class;
      Ps   : Leander.Core.Predicates.Predicate_Array)
   is
      Target : Predicate_Vectors.Vector renames
                 Dictionaries_Table (This.Dict_Id);
   begin
      Target.Clear;
      for P of Ps loop
         Target.Append (P);
      end loop;
   end Set_Dictionaries;

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Allocator.Allocate (Instance (This))));

   ----------------------
   -- Explicit_Binding --
   ----------------------

   function Explicit_Binding
     (Name   : Varid;
      Alts   : Leander.Core.Alts.Reference_Array;
      Scheme : Leander.Core.Schemes.Reference)
      return Reference
   is
   begin
      return Allocate
        (Instance'
           (Alts'Length, Name, Alts, Nullable_Scheme_Reference (Scheme),
            Monomorphic => False,
            Dict_Id     => Next_Dict_Id));
   end Explicit_Binding;

   -------------------
   -- Has_Reference --
   -------------------

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean
   is
   begin
      return (for some Alt of This.Alts => Alt.Has_Reference (To));
   end Has_Reference;

   ----------------------
   -- Implicit_Binding --
   ----------------------

   function Implicit_Binding
     (Name        : Varid;
      Alts        : Leander.Core.Alts.Reference_Array;
      Monomorphic : Boolean := False)
      return Reference
   is
   begin
      return Allocate
        (Instance'(Alts'Length, Name, Alts, null, Monomorphic,
                   Dict_Id => Next_Dict_Id));
   end Implicit_Binding;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      --  Dictionaries_Table is deliberately left alone: Allocator.Prune
      --  keeps protected bindings alive, and those keep their Dict_Id, so
      --  clearing the table here would dangle their index.  Reclaiming
      --  entries belongs with the wider allocation cleanup in issue #59.
      Allocator.Prune;
   end Prune;

   ------------
   -- Report --
   ------------

   procedure Report is
   begin
      Allocator.Report;
   end Report;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
      function Show_Alts (Index : Positive) return String;

      ---------------
      -- Show_Alts --
      ---------------

      function Show_Alts (Index : Positive) return String is
         Img : constant String := This.Alts (Index).Show;
      begin
         if Index < This.Alt_Count then
            return Img & ";" & Show_Alts (Index + 1);
         else
            return Img;
         end if;
      end Show_Alts;

   begin
      return Core.To_String (This.Name) & "=" & Show_Alts (1);
   end Show;

   -----------------
   -- To_Calculus --
   -----------------

   function To_Calculus
     (This          : Instance'Class;
      Types         : in out Leander.Core.Inference.Inference_Context'Class;
      Env           : not null access constant
        Leander.Environment.Abstraction'Class;
      Tie_Recursion : Boolean := True)
      return Leander.Calculus.Tree
   is
   begin
      if not This.Alts (1).Has_Pattern then
         declare
            E : constant Leander.Calculus.Tree :=
                  This.Alts (1).Expression.To_Calculus (Types, Env);
         begin
            if Tie_Recursion and then This.Is_Recursive then
               return This.Tie (E);
            end if;
            return E;
         end;
      elsif This.Alts (1).Pattern.Is_Variable then
         declare
            Pat : constant Leander.Core.Patterns.Reference :=
                    This.Alts (1).Pattern;
            E   : Leander.Calculus.Tree :=
                    This.Alts (1).Expression.To_Calculus (Types, Env);
         begin
            if Pat.Is_Wildcard then
               E :=
                 Leander.Calculus.Lambda
                   (Leander.Names.New_Name, E);
            else
               E :=
                 Leander.Calculus.Lambda
                   (Leander.Names.Leander_Name (Pat.Variable), E);
            end if;
            if Tie_Recursion
              and then not Pat.Has_Reference (This.Name)
              and then This.Is_Recursive
            then
               E := This.Tie (E);
            end if;

            return E;
         end;
      end if;

      declare
         Builder : Leander.Core.Alts.Compiler.Builder;
      begin
         Builder.Initialize (Types, Env);
         declare
            Name : constant String := Core.To_String (This.Name);
         begin
            --  A name the compiler made up -- for a case, an if or a
            --  lambda -- means nothing to the reader, so only a real one
            --  is given.
            Builder.Set_Failure_Message
              (This.Alts (This.Alts'First).Expression.Show_Location
               & ": Non-exhaustive patterns"
               & (if Name'Length > 0 and then Name (Name'First) /= '_'
                  then " in " & Name else ""));
         end;
         if Tie_Recursion and then This.Is_Recursive then
            Builder.Add_Name (This.Name);
         end if;
         Builder.Add (This.Alts);
         declare
            Base   : constant Natural := Types.Predicate_Count;
            Result : constant Leander.Calculus.Tree := Builder.To_Calculus;
            Raised : constant Leander.Core.Predicates.Predicate_Array :=
                       Builder.Raised_Predicates;
         begin
            Types.Save_Predicates (Raised (Base + 1 .. Raised'Last));
            return Result;
         end;
      end;
   end To_Calculus;

   ---------
   -- Tie --
   ---------

   function Tie
     (This : Instance'Class;
      Tree : Leander.Calculus.Tree)
      return Leander.Calculus.Tree
   is (Leander.Calculus.Apply
         (Leander.Calculus.Symbol ("Y"),
          Leander.Calculus.Lambda
            (Leander.Names.Leander_Name (This.Name), Tree)));

   -----------------
   -- Update_Type --
   -----------------

   procedure Update_Type
     (This    : Instance'Class;
      Context : Leander.Core.Inference.Inference_Context)
   is
   begin
      for Alt of This.Alts loop
         Context.Update_Type (Alt);
      end loop;
   end Update_Type;

end Leander.Core.Bindings;
