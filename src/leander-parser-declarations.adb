with Ada.Containers.Vectors;

with Leander.Core.Binding_Groups;
with Leander.Core.Kinds;
with Leander.Core.Predicates;
with Leander.Core.Schemes;
with Leander.Core.Type_Synonyms;
with Leander.Core.Types;
with Leander.Core.Tyvars;
with Leander.Data_Types;
with Leander.Data_Types.Builder;
with Leander.Names;
with Leander.Parser.Bindings;
with Leander.Parser.Expressions;
with Leander.Parser.Tokens;            use Leander.Parser.Tokens;
with Leander.Parser.Lexical;           use Leander.Parser.Lexical;
with Leander.Parser.Types;
with Leander.Source;

with Leander.Syntax.Bindings;
with Leander.Syntax.Classes;
with Leander.Syntax.Deriving;
with Leander.Syntax.Expressions;
with Leander.Syntax.Patterns;
with Leander.Syntax.Records;
with Leander.Syntax.Types;

package body Leander.Parser.Declarations is

   --  A data type declared with field labels, whose selector functions
   --  are added to its module's bindings once the module's declarations
   --  are all in, so that a clash with a binding of the same name is seen
   --  wherever in the module the binding is.
   type Selector_Source is
      record
         Data_Type : Leander.Data_Types.Reference;
         Location  : Leander.Source.Source_Location;
      end record;

   package Selector_Source_Vectors is
     new Ada.Containers.Vectors (Positive, Selector_Source);

   Pending_Selectors : Selector_Source_Vectors.Vector;

   procedure Add_Selectors
     (Bindings : Leander.Syntax.Bindings.Reference;
      Source   : Selector_Source);
   --  One function for each of Source's labels, selecting that field from
   --  whichever of its constructors has it (Haskell 2010, section 3.15.1).

   package Name_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Names.Leander_Name, Leander.Names."=");

   package Syntax_Type_Vectors is
     new Ada.Containers.Vectors
       (Positive, Leander.Syntax.Types.Reference,
        Leander.Syntax.Types."=");

   type Nullable_Core_Type is
     access constant Leander.Core.Types.Instance'Class;

   package Core_Type_Vectors is
     new Ada.Containers.Vectors (Positive, Nullable_Core_Type);

   procedure Parse_Class_Declaration
     (Context : in out Parse_Context'Class);

   procedure Parse_Data_Declaration
     (Context    : Parse_Context'Class;
      Is_Newtype : Boolean);
   --  Handles both 'data' and 'newtype': the grammar is the same, except
   --  that a newtype must declare exactly one constructor of exactly one
   --  field, which is what makes its zero-cost representation possible.

   procedure Parse_Foreign_Import
     (Context : Parse_Context'Class);

   procedure Parse_Type_Synonym_Declaration
     (Context : Parse_Context'Class);

   procedure Parse_Instance_Declaration
     (Context : in out Parse_Context'Class);

   procedure Parse_Constraints
     (Context        : Parse_Context'Class;
      Add_Constraint : not null access
        procedure (Constraint_Name : String;
                   Variable_Name   : String));

   procedure Skip_Declaration;

   -------------------
   -- Add_Selectors --
   -------------------

   procedure Add_Selectors
     (Bindings : Leander.Syntax.Bindings.Reference;
      Source   : Selector_Source)
   is
      DT  : constant Leander.Data_Types.Reference := Source.Data_Type;
      Loc : constant Leander.Source.Source_Location := Source.Location;
   begin
      for Index in 1 .. DT.Constructor_Count loop
         for Label_Name of DT.Constructor_Labels (Index) loop
            declare
               Label : constant String :=
                         Leander.Names.To_String (Label_Name);
            begin
               if (for some Earlier in 1 .. Index - 1 =>
                     DT.Label_Position (Earlier, Label) > 0)
               then
                  null;
               elsif Bindings.Binds (Label) then
                  Leander.Syntax.Records.Report
                    (Loc, "multiple declarations of " & Label);
               else
                  for Con in 1 .. DT.Constructor_Count loop
                     declare
                        Position : constant Natural :=
                                     DT.Label_Position (Con, Label);
                     begin
                        if Position > 0 then
                           Bindings.Add_Binding
                             (Loc, Label,
                              [Leander.Syntax.Patterns.Constructor
                                 (Loc,
                                  Leander.Core.To_String
                                    (DT.Constructor_Name (Con)),
                                  [for Arg in 1 .. DT.Constructor_Arity (Con)
                                   => (if Arg = Position
                                     then Leander.Syntax.Patterns.Variable
                                       (Loc, "x")
                                     else Leander.Syntax.Patterns.Wildcard
                                       (Loc))])],
                              Leander.Syntax.Expressions.Variable
                                (Loc, "x"));
                        end if;
                     end;
                  end loop;

                  if (for some Con in 1 .. DT.Constructor_Count =>
                        DT.Label_Position (Con, Label) = 0)
                  then
                     Bindings.Add_Binding
                       (Loc, Label,
                        [Leander.Syntax.Patterns.Wildcard (Loc)],
                        Leander.Syntax.Expressions.Application
                          (Loc,
                           Leander.Syntax.Expressions.Variable
                             (Loc, "error"),
                           Leander.Syntax.Expressions.String_Literal
                             (Loc, "No match in record selector " & Label)));
                  end if;
               end if;
            end;
         end loop;
      end loop;
   end Add_Selectors;

   --------------------
   -- At_Declaration --
   --------------------

   function At_Declaration return Boolean is
   begin
      --  Tok_Import is here only so that an import written after other
      --  declarations is reported as being in the wrong place rather than
      --  silently ending the module. Parse_Module has already consumed
      --  every correctly placed one.
      return At_Variable or else At_Constructor or else
        Tok <= [Tok_Class, Tok_Data, Tok_Foreign, Tok_Import,
                Tok_Infix, Tok_Infixl, Tok_Infixr,
                Tok_Instance, Tok_Newtype, Tok_Type,
                Tok_Left_Bracket, Tok_Left_Paren];
   end At_Declaration;

   -----------------------------
   -- Parse_Class_Declaration --
   -----------------------------

   procedure Parse_Class_Declaration
     (Context : in out Parse_Context'Class)
   is

      Builder : Leander.Syntax.Classes.Builder_Instance;

      procedure Add_Constraint
        (Constraint_Name : String;
         Variable_Name   : String);

      --------------------
      -- Add_Constraint --
      --------------------

      procedure Add_Constraint
        (Constraint_Name : String;
         Variable_Name   : String)
      is
         Name : constant Leander.Names.Leander_Name :=
                  Leander.Names.To_Leander_Name (Constraint_Name);
      begin
         if Context.Environment.Exists
           (Name, Leander.Environment.Class_Binding)
         then
            Builder.Add_Constraint (Constraint_Name, Variable_Name);
         else
            Error ("no such class: " & Constraint_Name);
         end if;
      end Add_Constraint;

      Indent : constant Positive := Tok_Indent;

   begin
      pragma Assert (Tok = Tok_Class);
      Scan;

      Parse_Constraints (Context, Add_Constraint'Access);

      if not At_Constructor_Name then
         Error ("missing class constructor");
         Skip_Declaration;
         return;
      end if;

      declare
         Name  : constant String := Scan_Identifier;
         Varid : constant String :=
                   (if At_Variable_Name
                    then Scan_Identifier
                    else "*");
         Tyvar : constant Leander.Core.Types.Reference :=
                   Leander.Core.Types.TVar
                     (Leander.Core.Tyvars.Tyvar
                        (Core.To_Varid (Varid), Leander.Core.Kinds.Star));

      begin
         if Varid = "*" then
            Error ("missing type variable");
         end if;

         Builder.Start_Class (Name, Varid);

         if Tok = Tok_Where then
            Scan;
         else
            Error ("missing 'where'");
            while Tok_Indent > 1 loop
               Scan;
            end loop;
            return;
         end if;

         declare
            Constraints : constant Leander.Core.Predicates.Predicate_Array :=
                            [Leander.Core.Predicates.Predicate
                               (Name, Tyvar)];
            Bindings    : constant Leander.Syntax.Bindings.Reference :=
                            Leander.Syntax.Bindings.Empty
                              (Leander.Core.Class_Context, Constraints);
         begin
            while Tok_Indent > Indent loop
               Leander.Parser.Bindings.Parse_Binding (Context, Bindings);
            end loop;

            Builder.Add_Bindings (Bindings.To_Core);
            Context.Environment.Type_Class (Builder.Get_Class);
         end;
      end;
   end Parse_Class_Declaration;

   -----------------------
   -- Parse_Constraints --
   -----------------------

   procedure Parse_Constraints
     (Context        : Parse_Context'Class;
      Add_Constraint : not null access
        procedure (Constraint_Name : String;
                   Variable_Name   : String))
   is
      function Is_Type_Class (Name : String) return Boolean
      is (Context.Environment.Exists
          (Leander.Names.To_Leander_Name (Name),
             Leander.Environment.Class_Binding));

   begin
      if At_Constructor_Name
        and then Next_Tok = Tok_Identifier
        and then Next_Tok (2) = Tok_Double_Right_Arrow
      then
         if Is_Type_Class (Tok_Text) then
            Add_Constraint (Tok_Text, Tok_Text (1));
         else
            Error ("no such class: " & Tok_Text);
         end if;

         Scan;
         if not At_Variable_Name then
            Error ("expected a type variable");
         end if;
         Scan;
         Scan;
      elsif Tok = Tok_Left_Paren then
         Scan;
         while At_Constructor_Name loop
            if not Is_Type_Class (Tok_Text) then
               Error ("no such class: " & Tok_Text);
            end if;
            declare
               Class_Name : constant String := Tok_Text;
            begin
               Scan;
               if At_Variable_Name then
                  declare
                     Var_Name : constant String := Tok_Text;
                  begin
                     Scan;
                     Add_Constraint (Class_Name, Var_Name);
                  end;
               else
                  Error ("expected a type variable");
               end if;
            end;

            if Tok = Tok_Comma then
               Scan;
               if not At_Constructor_Name then
                  Error ("missing class context");
               end if;
            elsif At_Constructor_Name then
               Error ("missing ','");
            end if;
         end loop;

         if Tok = Tok_Right_Paren then
            Scan;
         else
            Error ("missing ')' at " & Tok_Text);
         end if;

         if Tok = Tok_Double_Right_Arrow then
            Scan;
         else
            Error ("missing '=>'");
         end if;
      end if;
   end Parse_Constraints;

   ----------------------------
   -- Parse_Data_Declaration --
   ----------------------------

   procedure Parse_Data_Declaration
     (Context    : Parse_Context'Class;
      Is_Newtype : Boolean)
   is
      Builder         : Leander.Data_Types.Builder.Data_Type_Builder;
      Derived_Max     : constant := 16;
      Derived_Classes : Leander.Names.Name_Array (1 .. Derived_Max);
      Derived_Count   : Natural := 0;
      Derived_Loc     : Leander.Source.Source_Location :=
                          Leander.Source.No_Location;
      Keyword         : constant String :=
                          (if Is_Newtype then "newtype" else "data");
      Con_Count       : Natural := 0;
      Declared_At     : constant Leander.Source.Source_Location :=
                          Current_Source_Location;
      Field_Labels    : Name_Vectors.Vector;
      Field_Types     : Core_Type_Vectors.Vector;
      Field_Error     : Boolean := False;
      --  Set when the fields are declared wrongly, so that there are no
      --  selectors to be wrong as well.

      procedure Parse_Fields
        (Con_Args : in out Syntax_Type_Vectors.Vector;
         Labels   : in out Name_Vectors.Vector);
      --  { f1, f2 :: t1, f3 :: t2, ... }: the constructor's fields, in
      --  order, with the type of each.

      procedure Check_Field_Type
        (Label     : Leander.Names.Leander_Name;
         Core_Type : Leander.Core.Types.Reference);
      --  A label shared by several constructors has the same type in each
      --  (Haskell 2010, section 4.2.1).

      ----------------------
      -- Check_Field_Type --
      ----------------------

      procedure Check_Field_Type
        (Label     : Leander.Names.Leander_Name;
         Core_Type : Leander.Core.Types.Reference)
      is
         Index : constant Name_Vectors.Extended_Index :=
                   Field_Labels.Find_Index (Label);
      begin
         if Index = Name_Vectors.No_Index then
            Field_Labels.Append (Label);
            Field_Types.Append (Nullable_Core_Type (Core_Type));
         elsif not Leander.Core.Types.Equivalent
           (Field_Types (Index), Core_Type)
         then
            Error ("field " & Leander.Names.To_String (Label)
                   & " has a different type in another constructor");
            Field_Error := True;
         end if;
      end Check_Field_Type;

      ------------------
      -- Parse_Fields --
      ------------------

      procedure Parse_Fields
        (Con_Args : in out Syntax_Type_Vectors.Vector;
         Labels   : in out Name_Vectors.Vector)
      is
         Own_Labels : Name_Vectors.Vector;
      begin
         pragma Assert (Tok = Tok_Left_Brace);
         Scan;
         while Tok /= Tok_Right_Brace loop
            declare
               First : constant Positive := Natural (Labels.Length) + 1;
            begin
               loop
                  if not At_Variable_Name then
                     Error ("expected a field name");
                     raise Parse_Error;
                  end if;

                  declare
                     Label : constant Leander.Names.Leander_Name :=
                               Leander.Names.To_Leander_Name (Tok_Text);
                  begin
                     if Own_Labels.Contains (Label) then
                        Error ("field " & Tok_Text & " is declared twice");
                        Field_Error := True;
                     end if;
                     Own_Labels.Append (Label);
                     Labels.Append (Label);
                  end;
                  Scan;
                  exit when Tok /= Tok_Comma;
                  Scan;
               end loop;

               if Tok /= Tok_Colon_Colon then
                  Error ("expected '::'");
                  raise Parse_Error;
               end if;
               Scan;

               --  A strictness flag is accepted, and has no effect.
               if Tok = Tok_Identifier and then Tok_Text = "!" then
                  Scan;
               end if;

               declare
                  Field_Type : constant Leander.Syntax.Types.Reference :=
                                 Parser.Types.Parse_Type_Expression (Context);
                  Core_Type  : constant Leander.Core.Types.Reference :=
                                 Field_Type.To_Core;
               begin
                  for I in First .. Natural (Labels.Length) loop
                     Con_Args.Append (Field_Type);
                     Check_Field_Type (Labels (I), Core_Type);
                  end loop;
               end;
            end;
            exit when Tok /= Tok_Comma;
            Scan;
         end loop;

         if Tok = Tok_Right_Brace then
            Scan;
         else
            Error ("missing '}'");
            raise Parse_Error;
         end if;
      end Parse_Fields;

   begin
      pragma Assert (Tok = (if Is_Newtype then Tok_Newtype else Tok_Data));
      Scan;

      if not At_Constructor then
         Error ("expected a type constructor after '" & Keyword & "'");
         return;
      end if;

      --  Checked before the head is parsed: To_Core expands a synonym, so
      --  by the time there is a core type the name has already been
      --  rewritten into whatever it stands for.
      if Leander.Core.Type_Synonyms.Exists
        (Leander.Core.To_Conid (Tok_Text))
      then
         Error (Tok_Text & " is already a type synonym");
         Skip_Declaration;
         return;
      end if;

      declare
         TExpr : constant Leander.Syntax.Types.Reference :=
                   Leander.Parser.Types.Parse_Type_Expression (Context);
         Data  : constant Leander.Core.Types.Reference :=
                   TExpr.To_Core;
      begin

         if Tok /= Tok_Equal then
            Error ("expected '='");
            Skip_Declaration;
            return;
         end if;

         Scan;

         Builder.Start (Data, Is_Newtype => Is_Newtype);

         loop
            if not At_Constructor then
               Error ("expected constructor");
               while Tok /= Tok_Vertical_Bar
                 and then Tok_Indent > 1
               loop
                  Scan;
               end loop;
            else
               declare
                  Con_Name : constant String := Tok_Text;
                  Con_Args : Syntax_Type_Vectors.Vector;
                  Labels   : Name_Vectors.Vector;
                  Indent   : constant Positive := Tok_Indent;
               begin
                  Scan;
                  if Tok = Tok_Left_Brace then
                     Parse_Fields (Con_Args, Labels);
                  else
                     while Tok_Indent > Indent
                       and then Parser.Types.At_Atomic_Type loop
                        Con_Args.Append
                          (Parser.Types.Parse_Atomic_Type (Context));
                     end loop;
                  end if;

                  declare
                     Count    : constant Natural := Natural (Con_Args.Length);
                     Con_Type : Leander.Core.Types.Reference := Data;
                  begin
                     Con_Count := Con_Count + 1;
                     if Is_Newtype and then Count /= 1 then
                        Error
                          ("a newtype constructor takes exactly one"
                           & " argument; " & Con_Name & " takes"
                           & Natural'Image (Count));
                     end if;

                     for I in reverse 1 .. Count loop
                        Con_Type :=
                          Leander.Core.Types.Fn
                            (Con_Args.Element (I).To_Core, Con_Type);
                     end loop;

                     Builder.Add_Con
                       (Leander.Core.To_Conid (Con_Name),
                        Leander.Core.Schemes.Quantify
                          (Con_Type.Get_Tyvars, [], Con_Type),
                        Labels => [for Label of Labels => Label]);
                  end;
               end;
            end if;
            if Tok = Tok_Vertical_Bar then
               Scan;
            else
               exit;
            end if;
         end loop;

         if Is_Newtype and then Con_Count > 1 then
            Error ("a newtype declares exactly one constructor");
         end if;

         if Tok = Tok_Deriving then
            Derived_Loc := Current_Source_Location;
            Scan;

            declare
               procedure Record_Class;

               ------------------
               -- Record_Class --
               ------------------

               procedure Record_Class is
               begin
                  if Derived_Count = Derived_Max then
                     Error ("too many derived classes");
                  else
                     Derived_Count := Derived_Count + 1;
                     Derived_Classes (Derived_Count) :=
                       Leander.Names.To_Leander_Name (Tok_Text);
                  end if;
                  Scan;
               end Record_Class;
            begin
               if Tok = Tok_Left_Paren then
                  Scan;
                  if Tok /= Tok_Right_Paren then
                     loop
                        if not At_Constructor_Name then
                           Error ("expected a class name");
                           exit;
                        end if;
                        Record_Class;
                        exit when Tok /= Tok_Comma;
                        Scan;
                     end loop;
                  end if;
                  if Tok = Tok_Right_Paren then
                     Scan;
                  else
                     Error ("missing ')'");
                  end if;
               elsif At_Constructor_Name then
                  Record_Class;
               else
                  Error ("expected a class name or '('");
               end if;
            end;
         end if;

         Builder.Build;

         Context.Environment.Data_Type (Builder.Data_Type);

         if not Field_Labels.Is_Empty and then not Field_Error then
            Pending_Selectors.Append
              (Selector_Source'(Builder.Data_Type, Declared_At));
         end if;

         for I in 1 .. Derived_Count loop
            declare
               Class_Name : constant String :=
                              Leander.Names.To_String (Derived_Classes (I));
            begin
               if Class_Name = "Eq" then
                  Leander.Syntax.Deriving.Derive_Eq
                    (Context, Derived_Loc, Builder.Data_Type);
               elsif Class_Name = "Ord" then
                  Leander.Syntax.Deriving.Derive_Ord
                    (Context, Derived_Loc, Builder.Data_Type);
               elsif Class_Name = "Show" then
                  Leander.Syntax.Deriving.Derive_Show
                    (Context, Derived_Loc, Builder.Data_Type);
               elsif Class_Name = "Enum" then
                  Leander.Syntax.Deriving.Derive_Enum
                    (Context, Derived_Loc, Builder.Data_Type);
               elsif Class_Name = "Bounded" then
                  Leander.Syntax.Deriving.Derive_Bounded
                    (Context, Derived_Loc, Builder.Data_Type);
               elsif Class_Name = "Read" then
                  Leander.Syntax.Deriving.Derive_Read
                    (Context, Derived_Loc, Builder.Data_Type);
               else
                  Error ("cannot derive " & Class_Name);
               end if;
            end;
         end loop;
      end;

   end Parse_Data_Declaration;

   ------------------------------------
   -- Parse_Type_Synonym_Declaration --
   ------------------------------------

   procedure Parse_Type_Synonym_Declaration
     (Context : Parse_Context'Class)
   is
      Max_Parameters : constant := 16;
      Parameters     : Leander.Core.Varid_Array (1 .. Max_Parameters);
      Count          : Natural := 0;
   begin
      pragma Assert (Tok = Tok_Type);
      Scan;

      if not At_Constructor then
         Error ("expected a type constructor after 'type'");
         Skip_Declaration;
         return;
      end if;

      declare
         Name : constant String := Scan_Identifier;
      begin
         while At_Variable loop
            if Count = Max_Parameters then
               Error ("too many type synonym parameters");
               Skip_Declaration;
               return;
            end if;
            Count := Count + 1;
            Parameters (Count) :=
              Leander.Core.To_Varid (Scan_Identifier);
         end loop;

         if Tok /= Tok_Equal then
            Error ("expected '=' in type synonym declaration");
            Skip_Declaration;
            return;
         end if;

         Scan;

         if Context.Environment.Exists
           (Leander.Names.To_Leander_Name (Name),
            Leander.Environment.Type_Constructor)
         then
            Error (Name & " is already a data type");
            Skip_Declaration;
            return;
         end if;

         --  To_Core expands any synonym the right-hand side names, so what
         --  is stored is always fully expanded.  That is what lets Expand
         --  be a single rewrite rather than an iteration to a fixed point,
         --  and it means a synonym must be declared before it is used.
         declare
            Definition : constant Leander.Core.Types.Reference :=
                           Leander.Parser.Types.Parse_Type_Expression
                             (Context).To_Core;

            function Mentions
              (T : Leander.Core.Types.Reference)
               return Boolean
            is (if T.Is_Application
                then Mentions (T.Left) or else Mentions (T.Right)
                elsif T.Is_Constructor
                then Leander.Core.To_String (T.Constructor.Id) = Name
                else False);

         begin
            --  The right-hand side is expanded before it is stored, so a
            --  synonym naming itself has nothing to expand into and would
            --  otherwise be registered as a definition that goes nowhere.
            if Mentions (Definition) then
               Error ("type synonym " & Name & " is recursive");
               return;
            end if;

            Leander.Core.Type_Synonyms.Add
              (Name       => Leander.Core.To_Conid (Name),
               Parameters => Parameters (1 .. Count),
               Definition => Definition);
         end;
      end;
   end Parse_Type_Synonym_Declaration;

   ------------------------
   -- Parse_Declarations --
   ------------------------

   procedure Parse_Declarations
     (Context : in out Parse_Context'Class)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty;
      Selectors_From : constant Positive :=
                         Natural (Pending_Selectors.Length) + 1;

      procedure Take_Selectors;
      --  Add the selectors of this module's labelled data types to its
      --  bindings, and forget them.

      --------------------
      -- Take_Selectors --
      --------------------

      procedure Take_Selectors is
      begin
         for I in Selectors_From .. Natural (Pending_Selectors.Length) loop
            Add_Selectors (Bindings, Pending_Selectors (I));
         end loop;
         Pending_Selectors.Set_Length
           (Ada.Containers.Count_Type (Selectors_From - 1));
      end Take_Selectors;

   begin
      while At_Declaration loop
         if Leander.Parser.Bindings.At_Binding then
            Leander.Parser.Bindings.Parse_Binding (Context, Bindings);
         elsif Tok = Tok_Foreign then
            Scan;
            if Tok = Tok_Import then
               Scan;
               Parse_Foreign_Import (Context);
            else
               Error ("only foreign imports supported");
               while Tok_Indent > 1 loop
                  Scan;
               end loop;
            end if;
         elsif Tok = Tok_Import then
            Error ("import declarations must precede other declarations");
            --  Skip_Declaration advances only while the indent exceeds 1,
            --  and an import sits at column 1, so it would not move at all
            --  and At_Declaration would still be true: skip the line.
            declare
               Line : constant Natural := Tok_Line;
            begin
               while Tok /= Tok_End_Of_File
                 and then Tok_Line = Line
               loop
                  Scan;
               end loop;
            end;
         elsif Tok in Tok_Infix | Tok_Infixl | Tok_Infixr then
            declare
               Assoc    : constant Expressions.Associativity_Type :=
                            (if Tok = Tok_Infixl
                             then Expressions.Left
                             elsif Tok = Tok_Infixr
                             then Expressions.Right
                             else Expressions.None);
               Priority : Expressions.Priority_Range := 0;
            begin
               Scan;
               if Tok /= Tok_Integer_Literal then
                  Error ("expected an integer precedence");
               else
                  declare
                     P : constant Natural :=
                           Natural'Value (Tok_Text);
                  begin
                     if P in 0 .. 9 then
                        Priority := Expressions.Priority_Range (P);
                     else
                        Error ("priority must be in the range 0 .. 9");
                     end if;
                  end;
                  Scan;
               end if;

               Expressions.Add_Fixity (Scan_Identifier, Assoc, Priority);

               while Tok = Tok_Comma loop
                  Scan;
                  if not At_Operator then
                     Error ("operator expected");
                     while Tok_Indent > 1 and then not At_Operator loop
                        Scan;
                     end loop;
                  end if;
                  if At_Operator then
                     Expressions.Add_Fixity (Scan_Identifier, Assoc, Priority);
                  end if;
               end loop;
            end;
         elsif Tok = Tok_Data then
            Parse_Data_Declaration (Context, Is_Newtype => False);
         elsif Tok = Tok_Newtype then
            Parse_Data_Declaration (Context, Is_Newtype => True);
         elsif Tok = Tok_Type then
            Parse_Type_Synonym_Declaration (Context);
         elsif Tok = Tok_Class then
            Parse_Class_Declaration (Context);
         elsif Tok = Tok_Instance then
            Parse_Instance_Declaration (Context);
         else
            Error ("only bindings are supported");
            Scan;
            Skip_Declaration;
         end if;
      end loop;
      Take_Selectors;
      Context.Environment.Bindings (Bindings.To_Core);

   exception
      when Parse_Error =>
         Take_Selectors;
         --  The error has been reported, and the module counts as failed,
         --  but the environment still needs its bindings: elaborating it,
         --  and anything importing it, looks them up (issue #113).  It
         --  gets the ones parsed before the error, or none if even those
         --  cannot be made into a binding group -- a signature whose
         --  equations never arrived, say.
         declare
            Group : Leander.Core.Binding_Groups.Reference;
         begin
            begin
               Group := Bindings.To_Core;
            exception
               when others =>
                  Group := Leander.Syntax.Bindings.Empty.To_Core;
            end;
            Context.Environment.Bindings (Group);
         end;
         raise;
   end Parse_Declarations;

   --------------------------
   -- Parse_Foreign_Import --
   --------------------------

   procedure Parse_Foreign_Import
     (Context : Parse_Context'Class)
   is
      Import_Type  : constant String := Tok_Text;
   begin

      if Import_Type /= "skit" then
         Error ("only skit imports are supported");
      end if;

      Scan;

      if Tok = Tok_String_Literal then
         declare
            Foreign_Name : constant String := Tok_Text;
         begin
            Scan;

            if Tok = Tok_Identifier then
               declare
                  Local_Name : constant String := Tok_Text;
               begin
                  Scan;
                  if Tok = Tok_Colon_Colon then
                     Scan;
                     declare
                        Local_Type : constant Syntax.Types.Reference :=
                                       Parser.Types.Parse_Type_Expression
                                         (Context);
                     begin
                        Context.Environment.Foreign_Import
                          (Local_Name, Foreign_Name,
                           Local_Type.To_Core);
                     end;
                  else
                     Error ("missing import type");
                  end if;
               end;
            else
               Error ("missing name");
            end if;
         end;
      else
         Error ("missing local name");
      end if;
   end Parse_Foreign_Import;

   --------------------------------
   -- Parse_Instance_Declaration --
   --------------------------------

   procedure Parse_Instance_Declaration
     (Context : in out Parse_Context'Class)
   is
      Indent : constant Positive := Tok_Indent;
   begin
      pragma Assert (Tok = Tok_Instance);
      Scan;

      declare
         Constraint : constant Leander.Core.Predicates.Predicate_Array :=
                        (if Tok = Tok_Left_Paren
                         then Leander.Parser.Types.Parse_Constraint (Context)
                         else []);
      begin
         if Tok /= Tok_Identifier
           or else not At_Constructor
         then
            Error ("missing class name");
            Skip_Declaration;
            return;
         end if;

         if not Context.Environment.Exists
           (Leander.Names.To_Leander_Name (Tok_Text),
            Leander.Environment.Class_Binding)
         then
            Error ("unknown class: " & Tok_Text);
         end if;

         declare
            Class_Name    : constant String := Scan_Identifier;
            Instance_Type : constant Leander.Syntax.Types.Reference :=
                              Parser.Types.Parse_Type_Expression (Context);
         begin
            if Tok /= Tok_Where then
               Error ("missing 'where'");
               Skip_Declaration;
               return;
            end if;

            Scan;

            declare
               Bindings    : constant Leander.Syntax.Bindings.Reference :=
                               Leander.Syntax.Bindings.Empty
                                 (Leander.Core.Instance_Context, Constraint);
            begin
               while Tok_Indent > Indent loop
                  Leander.Parser.Bindings.Parse_Binding (Context, Bindings);
               end loop;

               --  Any method this instance omits falls back, at
               --  elaboration time, to the class's own generically
               --  compiled default (Leander.Environment.Type_Class /
               --  Elaborate_Instance) if it has one -- no AST splicing
               --  needed here, so this works identically whether the
               --  class came from source or from a .skix image.

               Context.Environment.Type_Instance
                 (Class_Id      => Core.To_Conid (Class_Name),
                  Constraints   => Constraint,
                  Instance_Type => Instance_Type.To_Core,
                  Bindings      => Bindings.To_Core);
            end;
         end;
      end;
   end Parse_Instance_Declaration;

   ----------------------
   -- Skip_Declaration --
   ----------------------

   procedure Skip_Declaration is
   begin
      while Tok_Indent > 1 loop
         Scan;
      end loop;
   end Skip_Declaration;

end Leander.Parser.Declarations;
