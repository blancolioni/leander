with Leander.Set_Of_Names;

with Leander.Source;

with Leander.Types.Bindings;

with Leander.Core.Cases;
with Leander.Core.Trees;

package body Leander.Types.Compiler is

   procedure Compile_Named_Field
     (Env        : in out Leander.Environments.Environment;
      Binding    : Leander.Types.Bindings.Type_Binding'Class;
      Field_Name : String);

   procedure Compile_Constructor_Field
     (Env        : in out Leander.Environments.Environment;
      Binding    : Leander.Types.Bindings.Type_Binding'Class;
      Con_Index  : Leander.Types.Bindings.Constructor_Index_Range;
      Field_Name : String);

   ----------------------------
   -- Compile_Algebraic_Type --
   ----------------------------

   procedure Compile_Algebraic_Type
     (Env  : in out Leander.Environments.Environment;
      Name : String)
   is
      use Leander.Core, Leander.Core.Trees;
      Done : Leander.Set_Of_Names.Set;
      Type_Binding : constant Leander.Types.Bindings.Type_Binding'Class :=
                       Env.Type_Constructor_Binding (Name);
      Source       : constant Leander.Source.Source_Reference :=
                       Type_Binding.Declaration;
   begin
      for I in 1 .. Type_Binding.Constructor_Count loop
         declare
            Name : constant String := Type_Binding.Constructor_Name (I);
            Arity : constant Natural :=
                      Type_Binding.Constructor_Arity (I);
            Con_Undefined : Tree_Type :=
                              Leaf (Constructor (Source, Name));

         begin
            for J in 1 .. Arity loop
               declare
                  Field_Name : constant String :=
                                 Type_Binding.Constructor_Field_Name (I, J);
               begin
                  if Field_Name /= ""
                    and then not Done.Contains (Field_Name)
                  then
                     Compile_Named_Field
                       (Env, Type_Binding, Field_Name);
                     Done.Insert (Field_Name);
                  end if;

                  if Field_Name /= "" then
                     Compile_Constructor_Field
                       (Env, Type_Binding, I, Field_Name);
                  end if;
               end;
               Con_Undefined :=
                 Apply (Con_Undefined, Variable (Source, "undefined"));

            end loop;

            Env.Insert_Value
              ("undefined-" & Name, Con_Undefined);
         end;
      end loop;
   end Compile_Algebraic_Type;

   -------------------------------
   -- Compile_Constructor_Field --
   -------------------------------

   procedure Compile_Constructor_Field
     (Env        : in out Leander.Environments.Environment;
      Binding    : Leander.Types.Bindings.Type_Binding'Class;
      Con_Index  : Leander.Types.Bindings.Constructor_Index_Range;
      Field_Name : String)
   is
      use Leander.Core, Leander.Core.Trees;
      Con_Name : constant String := Binding.Constructor_Name (Con_Index);
      Con_Arity : constant Natural := Binding.Constructor_Arity (Con_Index);
      Update_Name : constant String := Con_Name & "-update-" & Field_Name;
      Source       : constant Leander.Source.Source_Reference :=
                       Binding.Declaration;
      Builder      : Leander.Core.Cases.Case_Builder;
      Exp          : Tree_Type :=
                      Leaf (Constructor (Source, Con_Name));
      Pat          : Tree_Type :=
                       Leaf (Constructor (Source, Con_Name));
   begin
      Builder.Set_Case_Expression (Leaf (Variable (Source, "y")));
      for I in 1 .. Con_Arity loop
         if Binding.Constructor_Field_Name (Con_Index, I) = Field_Name then
            Pat :=
              Apply (Pat, Variable (Source, "_"));
            Exp := Apply (Exp, Variable (Source, "x"));
         else
            Pat :=
              Apply (Pat, Variable (Source, "x" & Integer'Image (-I)));
            Exp := Apply (Exp, Variable (Source, "x" & Integer'Image (-I)));
         end if;
      end loop;

      Builder.Add_Alt (Pat, Exp);

      Env.Insert_Value
        (Update_Name,
         Apply
           (Lambda (Source, "x"),
            Apply (Lambda (Source, "y"), Builder.Transform)));

   end Compile_Constructor_Field;

   -------------------------
   -- Compile_Named_Field --
   -------------------------

   procedure Compile_Named_Field
     (Env        : in out Leander.Environments.Environment;
      Binding    : Leander.Types.Bindings.Type_Binding'Class;
      Field_Name : String)
   is
      use Leander.Core, Leander.Core.Trees;
      Source : constant Leander.Source.Source_Reference :=
                 Binding.Declaration;
      Builder : Leander.Core.Cases.Case_Builder;
      Update_Builder : Leander.Core.Cases.Case_Builder;
      Var_Name : constant String := "x";
   begin
      Builder.Set_Case_Expression
        (Leaf (Variable (Binding.Declaration, Var_Name)));
      Update_Builder.Set_Case_Expression
        (Leaf (Variable (Binding.Declaration, "y")));

      for I in 1 .. Binding.Constructor_Count loop
         declare
            Arity       : constant Natural :=
                            Binding.Constructor_Arity (I);
            Con_Name    : constant String := Binding.Constructor_Name (I);
            Field_Index : Natural := 0;
         begin
            for J in 1 .. Arity loop
               if Binding.Constructor_Field_Name (I, J) = Field_Name then
                  Field_Index := J;
                  exit;
               end if;
            end loop;

            if Field_Index /= 0 then
               declare
                  Pat : Leander.Core.Trees.Tree_Type :=
                          Leander.Core.Trees.Leaf
                            (Leander.Core.Constructor
                               (Binding.Declaration,
                                Binding.Constructor_Name (I)));
               begin
                  for J in 1 .. Arity loop
                     declare
                        Name : constant String :=
                                 (if J = Field_Index then "y" else "_");
                     begin
                        Pat :=
                          Leander.Core.Trees.Apply
                            (Pat,
                             Leander.Core.Variable
                               (Binding.Declaration, Name));
                     end;
                  end loop;
                  Builder.Add_Alt
                    (Pat,
                     Leander.Core.Trees.Leaf
                       (Leander.Core.Variable
                            (Binding.Declaration, "y")));
               end;
               declare
                  Pat : Tree_Type :=
                          Leaf (Constructor (Source, Con_Name));
               begin
                  for J in 1 .. Arity loop
                     Pat := Apply (Pat, Variable (Source, "_"));
                  end loop;
                  Update_Builder.Add_Alt
                    (Pat,
                     Apply
                       (Apply
                            (Variable
                                 (Source,
                                  Con_Name & "-update-" & Field_Name),
                             Variable (Source, "x")),
                        Variable (Source, "y")));
               end;
            end if;
         end;
      end loop;

      declare
         Fn : Leander.Core.Trees.Tree_Type := Builder.Transform;
      begin
         Fn :=
           Leander.Core.Trees.Apply
             (Leander.Core.Lambda (Binding.Declaration, "x"), Fn);
         Env.Insert_Value (Field_Name, Fn);
      end;

      Env.Insert_Value
        ("update-" & Field_Name,
         Apply (Lambda (Source, "x"),
           Apply (Lambda (Source, "y"), Update_Builder.Transform)));

   end Compile_Named_Field;

end Leander.Types.Compiler;
