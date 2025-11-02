with Leander.Allocator;
with Leander.Core.Alts.Compiler;

package body Leander.Core.Bindings is

   type Variable_Reference is access all Instance;

   package Allocator is
     new Leander.Allocator ("bindings", Instance, Variable_Reference);

   function Allocate
     (This : Instance'Class)
      return Reference
   is (Reference (Allocator.Allocate (Instance (This))));

   ----------------------
   -- Implicit_Binding --
   ----------------------

   function Implicit_Binding
     (Name : Varid;
      Alts : Leander.Core.Alts.Reference_Array)
      return Reference
   is
   begin
      return Allocate (Instance'(Alts'Length, Name, Alts, null));
   end Implicit_Binding;

   -----------
   -- Prune --
   -----------

   procedure Prune is
   begin
      Allocator.Prune;
   end Prune;

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
     (This  : Instance'Class;
      Types : Leander.Core.Inference.Inference_Context'Class;
      Env   : not null access constant Leander.Environment.Abstraction'Class)
      return Leander.Calculus.Tree
   is
   begin
      if This.Alt_Count = 1
        and then This.Alts (1).Patterns'Length = 0
      then
         return This.Alts (1).Expression.To_Calculus (Types, Env);
      end if;

      if This.Alt_Count = 1
        and then (for all Pat of This.Alts (1).Patterns =>
                      Pat.Is_Variable)
      then
         declare
            E : Leander.Calculus.Tree :=
                  This.Alts (1).Expression.To_Calculus (Types, Env);
         begin
            for Pat of reverse This.Alts (1).Patterns loop
               if Pat.Is_Wildcard then
                  E :=
                    Leander.Calculus.Lambda
                      (Leander.Names.New_Name, E);
               else
                  E :=
                    Leander.Calculus.Lambda
                      (Leander.Names.Leander_Name (Pat.Variable), E);
               end if;
            end loop;
            return E;
         end;
      end if;

      declare
         Builder : Leander.Core.Alts.Compiler.Builder;
      begin
         Builder.Initialize (Types, Env);
         if (for some Alt of This.Alts => Alt.Has_Reference (This.Name)) then
            Builder.Add_Name (This.Name);
         end if;
         Builder.Add (This.Alts);
         return Builder.To_Calculus;
      end;
   end To_Calculus;

end Leander.Core.Bindings;
