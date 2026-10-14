with Ada.Strings.Fixed;

with Leander.Core;
with Leander.Core.Predicates;
with Leander.Core.Tyvars;
with Leander.Core.Types;

with Leander.Errors;
with Leander.Names;

with Leander.Syntax.Bindings;
with Leander.Syntax.Expressions;
with Leander.Syntax.Patterns;

package body Leander.Syntax.Deriving is

   subtype Expression is Leander.Syntax.Expressions.Reference;
   subtype Pattern is Leander.Syntax.Patterns.Reference;
   subtype Pattern_Array is Leander.Syntax.Patterns.Reference_Array;

   --  Building blocks for the generated methods, all at the location of
   --  the deriving clause.

   function Fresh return String
   is (Leander.Names.To_String (Leander.Names.New_Name));

   function Var_Expr
     (Loc  : Leander.Source.Source_Location;
      Name : String)
      return Expression
   is (Leander.Syntax.Expressions.Variable (Loc, Name));

   function Con_Expr
     (Loc  : Leander.Source.Source_Location;
      Name : String)
      return Expression
   is (Leander.Syntax.Expressions.Constructor (Loc, Name));

   function App
     (Loc  : Leander.Source.Source_Location;
      F, X : Expression)
      return Expression
   is (Leander.Syntax.Expressions.Application (Loc, F, X));

   function Op
     (Loc  : Leander.Source.Source_Location;
      Name : String;
      L, R : Expression)
      return Expression
   is (App (Loc, App (Loc, Var_Expr (Loc, Name), L), R));

   function Int_Expr
     (Loc   : Leander.Source.Source_Location;
      Value : Natural)
      return Expression
   is (Leander.Syntax.Expressions.Integer_Literal
         (Loc, Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left)));

   function String_Expr
     (Loc   : Leander.Source.Source_Location;
      Value : String)
      return Expression
   is (Leander.Syntax.Expressions.String_Literal (Loc, Value));

   function Pat_Var
     (Loc  : Leander.Source.Source_Location;
      Name : String)
      return Pattern
   is (Leander.Syntax.Patterns.Variable (Loc, Name));

   function Pat_Con
     (Loc  : Leander.Source.Source_Location;
      Name : String;
      Args : Pattern_Array)
      return Pattern
   is (Leander.Syntax.Patterns.Constructor (Loc, Name, Args));

   function Pat_Int
     (Loc   : Leander.Source.Source_Location;
      Value : Natural)
      return Pattern
   is (Leander.Syntax.Patterns.Integer_Literal
         (Loc, Ada.Strings.Fixed.Trim (Value'Image, Ada.Strings.Left)));

   function Con_Name
     (DT    : Leander.Data_Types.Reference;
      Index : Positive)
      return String
   is (Leander.Core.To_String (DT.Constructor_Name (Index)));

   function Is_Enumeration
     (DT : Leander.Data_Types.Reference)
      return Boolean
   is (for all I in 1 .. DT.Constructor_Count =>
         DT.Constructor_Arity (I) = 0);

   subtype Name_List is Leander.Names.Name_Array;

   function Fresh_Names (Count : Natural) return Name_List
   is ([for I in 1 .. Count => Leander.Names.New_Name]);

   function Image (Name : Leander.Names.Leander_Name) return String
   renames Leander.Names.To_String;

   function Var_Pats
     (Loc   : Leander.Source.Source_Location;
      Names : Name_List)
      return Pattern_Array
   is ([for N of Names => Pat_Var (Loc, Image (N))]);

   procedure Add_Instance
     (Context  : Leander.Parser.Parse_Context'Class;
      Class    : String;
      DT       : Leander.Data_Types.Reference;
      Bindings : Leander.Syntax.Bindings.Reference;
      Context_Class : String);
   --  Add the instance of Class for DT that Bindings define.  Its context
   --  asks Context_Class of each of DT's parameters, or nothing when
   --  Context_Class is empty.

   procedure Not_Derivable
     (Loc     : Leander.Source.Source_Location;
      Class   : String;
      DT      : Leander.Data_Types.Reference;
      Because : String);

   ------------------
   -- Add_Instance --
   ------------------

   procedure Add_Instance
     (Context  : Leander.Parser.Parse_Context'Class;
      Class    : String;
      DT       : Leander.Data_Types.Reference;
      Bindings : Leander.Syntax.Bindings.Reference;
      Context_Class : String)
   is
      Tyvars      : constant Leander.Core.Tyvars.Tyvar_Array :=
                      DT.Applied_Type.Get_Tyvars;
      Constraints : constant Leander.Core.Predicates.Predicate_Array :=
                      (if Context_Class = ""
                       then []
                       else [for Tv of Tyvars =>
                               Leander.Core.Predicates.Predicate
                                 (Context_Class,
                                  Leander.Core.Types.TVar (Tv))]);
   begin
      Context.Environment.Type_Instance
        (Class_Id      => Leander.Core.To_Conid (Class),
         Constraints   => Constraints,
         Instance_Type => DT.Applied_Type,
         Bindings      => Bindings.To_Core);
   end Add_Instance;

   -------------------
   -- Not_Derivable --
   -------------------

   procedure Not_Derivable
     (Loc     : Leander.Source.Source_Location;
      Class   : String;
      DT      : Leander.Data_Types.Reference;
      Because : String)
   is
   begin
      Leander.Errors.Report
        (Leander.Source.Simple_File_Name (Loc),
         Leander.Source.Line_Number (Loc),
         Leander.Source.Column_Number (Loc),
         "cannot derive " & Class & " for "
         & Leander.Core.To_String (DT.Id) & ": " & Because);
   end Not_Derivable;

   ---------------
   -- Derive_Eq --
   ---------------

   procedure Derive_Eq
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty
                     (Leander.Core.Instance_Context, []);
   begin
      for I in 1 .. DT.Constructor_Count loop
         declare
            L_Names  : constant Name_List :=
                         Fresh_Names (DT.Constructor_Arity (I));
            Y_Name   : constant String := Fresh;
            Fn_Name  : constant String := Fresh;
            Inner_Bs : constant Leander.Syntax.Bindings.Reference :=
                         Leander.Syntax.Bindings.Empty;
         begin
            for J in 1 .. DT.Constructor_Count loop
               declare
                  R_Names : constant Name_List :=
                              Fresh_Names (DT.Constructor_Arity (J));
                  Body_E  : Expression;

                  function Field_Equal (K : Positive) return Expression
                  is (Op (Loc, "==",
                          Var_Expr (Loc, Image (L_Names (K))),
                          Var_Expr (Loc, Image (R_Names (K)))));

               begin
                  if I /= J then
                     Body_E := Con_Expr (Loc, "False");
                  elsif L_Names'Length = 0 then
                     Body_E := Con_Expr (Loc, "True");
                  else
                     Body_E := Field_Equal (L_Names'Last);
                     for K in reverse 1 .. L_Names'Last - 1 loop
                        Body_E := Op (Loc, "&&", Field_Equal (K), Body_E);
                     end loop;
                  end if;

                  Inner_Bs.Add_Binding
                    (Loc, Fn_Name,
                     [Pat_Con (Loc, Con_Name (DT, J),
                               Var_Pats (Loc, R_Names))],
                     Body_E);
               end;
            end loop;

            Bindings.Add_Binding
              (Loc,
               Leander.Syntax.Bindings.Create_Binding_LHS
                 ("==",
                  [Pat_Con (Loc, Con_Name (DT, I), Var_Pats (Loc, L_Names)),
                   Pat_Var (Loc, Y_Name)]),
               Leander.Syntax.Expressions.Let
                 (Loc, Inner_Bs,
                  App (Loc, Var_Expr (Loc, Fn_Name),
                       Var_Expr (Loc, Y_Name))));
         end;
      end loop;

      declare
         X_Name : constant String := Fresh;
         Y_Name : constant String := Fresh;
      begin
         Bindings.Add_Binding
           (Loc,
            Leander.Syntax.Bindings.Create_Binding_LHS
              ("/=", [Pat_Var (Loc, X_Name), Pat_Var (Loc, Y_Name)]),
            App (Loc, Var_Expr (Loc, "not"),
                 Op (Loc, "==",
                     Var_Expr (Loc, X_Name), Var_Expr (Loc, Y_Name))));
      end;

      Add_Instance (Context, "Eq", DT, Bindings, "Eq");
   end Derive_Eq;

   ----------------
   -- Derive_Ord --
   ----------------

   procedure Derive_Ord
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty
                     (Leander.Core.Instance_Context, []);
   begin
      --  compare (Ci l1 .. ln) y = let f (Cj r1 .. rm) = ... in f y, where
      --  ... is LT or GT by the constructors' order, or for i = j the
      --  fields' comparisons chained: the first that is not EQ decides.
      for I in 1 .. DT.Constructor_Count loop
         declare
            L_Names  : constant Name_List :=
                         Fresh_Names (DT.Constructor_Arity (I));
            Y_Name   : constant String := Fresh;
            Fn_Name  : constant String := Fresh;
            Then_Cmp : constant String := Fresh;
            Inner_Bs : constant Leander.Syntax.Bindings.Reference :=
                         Leander.Syntax.Bindings.Empty;
         begin
            for J in 1 .. DT.Constructor_Count loop
               declare
                  R_Names : constant Name_List :=
                              Fresh_Names (DT.Constructor_Arity (J));
                  Body_E  : Expression;

                  function Field_Compare (K : Positive) return Expression
                  is (Op (Loc, "compare",
                          Var_Expr (Loc, Image (L_Names (K))),
                          Var_Expr (Loc, Image (R_Names (K)))));

               begin
                  if I < J then
                     Body_E := Con_Expr (Loc, "LT");
                  elsif I > J then
                     Body_E := Con_Expr (Loc, "GT");
                  elsif L_Names'Length = 0 then
                     Body_E := Con_Expr (Loc, "EQ");
                  else
                     Body_E := Field_Compare (L_Names'Last);
                     for K in reverse 1 .. L_Names'Last - 1 loop
                        Body_E :=
                          Op (Loc, Then_Cmp, Field_Compare (K), Body_E);
                     end loop;
                  end if;

                  Inner_Bs.Add_Binding
                    (Loc, Fn_Name,
                     [Pat_Con (Loc, Con_Name (DT, J),
                               Var_Pats (Loc, R_Names))],
                     Body_E);
               end;
            end loop;

            if DT.Constructor_Arity (I) > 1 then
               --  then EQ o = o; then o _ = o.  The Prelude's own is not
               --  exported, so each instance carries one.
               declare
                  O_Name : constant String := Fresh;
               begin
                  Inner_Bs.Add_Binding
                    (Loc, Then_Cmp,
                     [Pat_Con (Loc, "EQ", []), Pat_Var (Loc, O_Name)],
                     Var_Expr (Loc, O_Name));
                  Inner_Bs.Add_Binding
                    (Loc, Then_Cmp,
                     [Pat_Var (Loc, O_Name),
                      Leander.Syntax.Patterns.Wildcard (Loc)],
                     Var_Expr (Loc, O_Name));
               end;
            end if;

            Bindings.Add_Binding
              (Loc,
               Leander.Syntax.Bindings.Create_Binding_LHS
                 ("compare",
                  [Pat_Con (Loc, Con_Name (DT, I), Var_Pats (Loc, L_Names)),
                   Pat_Var (Loc, Y_Name)]),
               Leander.Syntax.Expressions.Let
                 (Loc, Inner_Bs,
                  App (Loc, Var_Expr (Loc, Fn_Name),
                       Var_Expr (Loc, Y_Name))));
         end;
      end loop;

      Add_Instance (Context, "Ord", DT, Bindings, "Ord");
   end Derive_Ord;

   -----------------
   -- Derive_Show --
   -----------------

   procedure Derive_Show
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty
                     (Leander.Core.Instance_Context, []);
   begin
      --  showsPrec _ C = showString "C"
      --  showsPrec d (C x1 .. xn) =
      --    showParen (d > 10)
      --      (showString "C " . showsPrec 11 x1 . showChar ' ' . ...)
      for I in 1 .. DT.Constructor_Count loop
         declare
            Name    : constant String := Con_Name (DT, I);
            X_Names : constant Name_List :=
                        Fresh_Names (DT.Constructor_Arity (I));
            D_Name  : constant String := Fresh;
            Body_E  : Expression;
         begin
            if X_Names'Length = 0 then
               Body_E := App (Loc, Var_Expr (Loc, "showString"),
                              String_Expr (Loc, Name));
            else
               Body_E :=
                 Op (Loc, "showsPrec", Int_Expr (Loc, 11),
                     Var_Expr (Loc, Image (X_Names (X_Names'Last))));
               for K in reverse 1 .. X_Names'Last - 1 loop
                  Body_E :=
                    Op (Loc, ".",
                        Op (Loc, "showsPrec", Int_Expr (Loc, 11),
                            Var_Expr (Loc, Image (X_Names (K)))),
                        Op (Loc, ".",
                            App (Loc, Var_Expr (Loc, "showString"),
                                 String_Expr (Loc, " ")),
                            Body_E));
               end loop;
               Body_E :=
                 Op (Loc, "showParen",
                     Op (Loc, ">", Var_Expr (Loc, D_Name), Int_Expr (Loc, 10)),
                     Op (Loc, ".",
                         App (Loc, Var_Expr (Loc, "showString"),
                              String_Expr (Loc, Name & " ")),
                         Body_E));
            end if;

            Bindings.Add_Binding
              (Loc,
               Leander.Syntax.Bindings.Create_Binding_LHS
                 ("showsPrec",
                  [(if X_Names'Length = 0
                    then Leander.Syntax.Patterns.Wildcard (Loc)
                    else Pat_Var (Loc, D_Name)),
                   Pat_Con (Loc, Name, Var_Pats (Loc, X_Names))]),
               Body_E);
         end;
      end loop;

      Add_Instance (Context, "Show", DT, Bindings, "Show");
   end Derive_Show;

   -----------------
   -- Derive_Enum --
   -----------------

   procedure Derive_Enum
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty
                     (Leander.Core.Instance_Context, []);
      Count    : constant Natural := DT.Constructor_Count;
   begin
      if not Is_Enumeration (DT) then
         Not_Derivable (Loc, "Enum", DT, "a constructor has fields");
         return;
      end if;

      --  fromEnum Ci = i, and toEnum i = Ci, counting from 0.
      for I in 1 .. Count loop
         Bindings.Add_Binding
           (Loc, "fromEnum", [Pat_Con (Loc, Con_Name (DT, I), [])],
            Int_Expr (Loc, I - 1));
      end loop;

      for I in 1 .. Count loop
         Bindings.Add_Binding
           (Loc, "toEnum", [Pat_Int (Loc, I - 1)],
            Con_Expr (Loc, Con_Name (DT, I)));
      end loop;

      Bindings.Add_Binding
        (Loc, "toEnum", [Leander.Syntax.Patterns.Wildcard (Loc)],
         App (Loc, Var_Expr (Loc, "error"),
              String_Expr
                (Loc,
                 "toEnum: no " & Leander.Core.To_String (DT.Id)
                 & " has that number")));

      --  enumFrom x = enumFromTo x Last
      declare
         X_Name : constant String := Fresh;
      begin
         Bindings.Add_Binding
           (Loc, "enumFrom", [Pat_Var (Loc, X_Name)],
            Op (Loc, "enumFromTo", Var_Expr (Loc, X_Name),
                Con_Expr (Loc, Con_Name (DT, Count))));
      end;

      --  enumFromThen x y = enumFromThenTo x y (limit (fromEnum y >=
      --  fromEnum x)), where limit True = Last and limit False = First:
      --  counting up stops at the last constructor, counting down at the
      --  first.
      declare
         X_Name   : constant String := Fresh;
         Y_Name   : constant String := Fresh;
         Limit    : constant String := Fresh;
         Inner_Bs : constant Leander.Syntax.Bindings.Reference :=
                      Leander.Syntax.Bindings.Empty;
      begin
         Inner_Bs.Add_Binding
           (Loc, Limit, [Pat_Con (Loc, "True", [])],
            Con_Expr (Loc, Con_Name (DT, Count)));
         Inner_Bs.Add_Binding
           (Loc, Limit, [Pat_Con (Loc, "False", [])],
            Con_Expr (Loc, Con_Name (DT, 1)));
         Bindings.Add_Binding
           (Loc, "enumFromThen",
            [Pat_Var (Loc, X_Name), Pat_Var (Loc, Y_Name)],
            Leander.Syntax.Expressions.Let
              (Loc, Inner_Bs,
               App (Loc,
                    Op (Loc, "enumFromThenTo",
                        Var_Expr (Loc, X_Name), Var_Expr (Loc, Y_Name)),
                    App (Loc, Var_Expr (Loc, Limit),
                         Op (Loc, ">=",
                             App (Loc, Var_Expr (Loc, "fromEnum"),
                                  Var_Expr (Loc, Y_Name)),
                             App (Loc, Var_Expr (Loc, "fromEnum"),
                                  Var_Expr (Loc, X_Name)))))));
      end;

      Add_Instance (Context, "Enum", DT, Bindings, "");
   end Derive_Enum;

   --------------------
   -- Derive_Bounded --
   --------------------

   procedure Derive_Bounded
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference)
   is
      Bindings : constant Leander.Syntax.Bindings.Reference :=
                   Leander.Syntax.Bindings.Empty
                     (Leander.Core.Instance_Context, []);
   begin
      if Is_Enumeration (DT) then
         Bindings.Add_Binding
           (Loc, "minBound", [], Con_Expr (Loc, Con_Name (DT, 1)));
         Bindings.Add_Binding
           (Loc, "maxBound", [],
            Con_Expr (Loc, Con_Name (DT, DT.Constructor_Count)));
         Add_Instance (Context, "Bounded", DT, Bindings, "");

      elsif DT.Constructor_Count = 1 then
         --  minBound = C minBound .. minBound, and so for maxBound.
         declare
            procedure Add_Bound (Bound : String);

            ---------------
            -- Add_Bound --
            ---------------

            procedure Add_Bound (Bound : String) is
               Value : Expression := Con_Expr (Loc, Con_Name (DT, 1));
            begin
               for K in 1 .. DT.Constructor_Arity (1) loop
                  Value := App (Loc, Value, Var_Expr (Loc, Bound));
               end loop;
               Bindings.Add_Binding (Loc, Bound, [], Value);
            end Add_Bound;

         begin
            Add_Bound ("minBound");
            Add_Bound ("maxBound");
         end;
         Add_Instance (Context, "Bounded", DT, Bindings, "Bounded");

      else
         Not_Derivable
           (Loc, "Bounded", DT,
            "it has more than one constructor, and one of them has fields");
      end if;
   end Derive_Bounded;

end Leander.Syntax.Deriving;
