with Ada.Containers.Vectors;

with SK.Objects.Bindings;
with SK.Objects.Symbols;

with Leander.Core;
with Leander.Kinds.Trees;

with Leander.Primitives.Large_Integers;

with Leander.Logging;

package body Leander.Primitives is

   Local_Trivial_Type : Leander.Types.Trees.Tree_Type;
   Local_Trivial_Con  : Leander.Types.Trees.Tree_Type;
   Local_Int_Type     : Leander.Types.Trees.Tree_Type;
   Local_Integer_Type : Leander.Types.Trees.Tree_Type;
   Local_Char_Type    : Leander.Types.Trees.Tree_Type;
   Local_List_Type    : Leander.Types.Trees.Tree_Type;
   Local_Empty_List   : Leander.Types.Trees.Tree_Type;
   Local_Cons         : Leander.Types.Trees.Tree_Type;
   Local_Map_Type     : Leander.Types.Trees.Tree_Type;
   Local_World_Type   : Leander.Types.Trees.Tree_Type;

   type Tuple_Entry_Record is
      record
         Type_Con  : Leander.Types.Trees.Tree_Type :=
                       Leander.Types.Trees.Empty;
         Value_Con : Leander.Types.Trees.Tree_Type :=
                       Leander.Types.Trees.Empty;
      end record;

   package Tuple_Vectors is
     new Ada.Containers.Vectors (Positive, Tuple_Entry_Record);

   Tuples : Tuple_Vectors.Vector;

   procedure Check_Tuple (Arity : Positive);

   function Object_To_String
     (Store   : SK.Objects.Object_Store'Class;
      Value   : SK.Objects.Object)
      return String;

   function Evaluate_Error
     (Store : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is (raise Evaluation_Error with
       Object_To_String (Store, Store.Argument (1)));

   function Evaluate_Int_From_Integer
     (Store : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object;

   function Evaluate_Int_To_Integer
     (Store : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object;

   ---------------
   -- Char_Type --
   ---------------

   function Char_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Char_Type;
   end Char_Type;

   -----------------
   -- Check_Tuple --
   -----------------

   procedure Check_Tuple (Arity : Positive) is
   begin
      while Tuples.Last_Index < Arity loop
         Tuples.Append ((others => <>));
      end loop;
      if Tuples.Element (Arity).Type_Con.Is_Empty then
         declare
            Name  : constant String := Tuple_Name (Arity);
            Tycon : Leander.Types.Trees.Tree_Type;
            Con   : Leander.Types.Trees.Tree_Type;
            Kind  : Leander.Kinds.Trees.Tree_Type :=
                      Leander.Kinds.Trees.Leaf
                        (Leander.Kinds.Primitive);
         begin
            for I in 1 .. Arity loop
               Kind :=
                 Leander.Kinds.Trees.Apply
                   (Leander.Kinds.Trees.Apply
                      (Leander.Kinds.Map,
                       Leander.Kinds.Primitive),
                    Kind);
            end loop;

            Tycon :=
              Leander.Types.Trees.Leaf
                (Leander.Types.Constructor (Name));
            Tycon.Set_Annotation (Kind);

            Con := Tycon;
            for I in 1 .. Arity loop
               Con :=
                 Leander.Types.Trees.Apply
                   (Con,
                    Leander.Types.Variable
                      ((1 => Character'Val (96 + I))));
               Con.Right.Set_Annotation
                 (Leander.Kinds.Trees.Leaf
                    (Leander.Kinds.Primitive));
            end loop;
            for I in reverse 1 .. Arity loop
               Con :=
                 Leander.Types.Trees.Apply
                   (Leander.Types.Trees.Apply
                      (Leander.Core.Map_Operator,
                       Leander.Types.Variable
                         ((1 => Character'Val (96 + I)))),
                    Con);
               Con.Right.Set_Annotation
                 (Leander.Kinds.Trees.Leaf
                    (Leander.Kinds.Primitive));
            end loop;

            Leander.Logging.Log
              (Tycon.Show & " :: " & Tycon.Annotation.Show);
            Leander.Logging.Log
              (Con.Show);

            Tuples (Arity) :=
              Tuple_Entry_Record'(Type_Con  => Tycon,
                                  Value_Con => Con);
         end;
      end if;
   end Check_Tuple;

   ----------
   -- Cons --
   ----------

   function Cons return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Cons;
   end Cons;

   ----------------
   -- Empty_List --
   ----------------

   function Empty_List return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Empty_List;
   end Empty_List;

   -------------------------------
   -- Evaluate_Int_From_Integer --
   -------------------------------

   function Evaluate_Int_From_Integer
     (Store : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is
      pragma Assert (SK.Objects.Is_External_Object (Store.Argument (1)));
      Ext_Obj : constant access SK.Objects.External_Object_Interface'Class :=
                  Store.Get_External_Object (Store.Argument (1));
      pragma Assert (Ext_Obj.all in Large_Integers.Large_Integer_Object'Class);
      Large_Int : constant access Large_Integers.Large_Integer_Object'Class :=
                    Large_Integers.Large_Integer_Object'Class
                      (Ext_Obj.all)'Access;
   begin
      if not Large_Int.In_Integer_Range then
         raise Constraint_Error with "out of range for Int: "
           & Large_Int.Print (Store);
      end if;
      return SK.Objects.To_Object (Large_Int.To_Integer);
   end Evaluate_Int_From_Integer;

   -----------------------------
   -- Evaluate_Int_To_Integer --
   -----------------------------

   function Evaluate_Int_To_Integer
     (Store : in out SK.Objects.Object_Store'Class)
      return SK.Objects.Object
   is
   begin
      return Store.Create_External_Reference
        (Leander.Primitives.Large_Integers.To_Large_Integer
           (SK.Objects.To_Integer (Store.Argument (1))));
   end Evaluate_Int_To_Integer;

   --------------
   -- Int_Type --
   --------------

   function Int_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Int_Type;
   end Int_Type;

   ------------------
   -- Integer_Type --
   ------------------

   function Integer_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Integer_Type;
   end Integer_Type;

   ---------------
   -- List_Type --
   ---------------

   function List_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_List_Type;
   end List_Type;

   ------------------------
   -- Load_SK_Primitives --
   ------------------------

   procedure Load_SK_Primitives
     (Store : in out SK.Objects.Object_Store'Class)
   is
      procedure Bind
        (Name      : String;
         Arg_Count : Natural;
         Eval      : SK.Objects.Bindings.General_Function_Handler);

      procedure Bind
        (Name      : String;
         Arg_Count : Natural;
         Eval      : SK.Objects.Bindings.General_Function_Handler)
      is
         Id : constant SK.Objects.Function_Id :=
                SK.Objects.Bindings.Add_Binding (Eval, Arg_Count);
      begin
         Store.Define_Symbol
           (SK.Objects.Symbols.Get_Symbol_Id (Name),
            SK.Objects.To_Object (Id));
      end Bind;

   begin
      Bind ("#error", 1, Evaluate_Error'Access);
      Bind ("#intFromInteger", 1, Evaluate_Int_From_Integer'Access);
      Bind ("#intToInteger", 1, Evaluate_Int_To_Integer'Access);
   end Load_SK_Primitives;

   --------------
   -- Map_Type --
   --------------

   function Map_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Map_Type;
   end Map_Type;

   ----------------------
   -- Object_To_String --
   ----------------------

   function Object_To_String
     (Store   : SK.Objects.Object_Store'Class;
      Value   : SK.Objects.Object)
      return String
   is

      use SK.Objects;

      function To_String (Value : SK.Objects.Object) return String
      is (if Is_Application (Value)
          then To_String (Store.Left (Value))
          & To_String (Store.Right (Value))
          elsif Is_Integer (Value)
          then (1 => Character'Val (To_Integer (Value)))
          else "");

   begin
      return To_String (Value);
   end Object_To_String;

   -----------------
   -- Trivial_Con --
   -----------------

   function Trivial_Con return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Trivial_Con;
   end Trivial_Con;

   ------------------
   -- Trivial_Type --
   ------------------

   function Trivial_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_Trivial_Type;
   end Trivial_Type;

   ---------------
   -- Tuple_Con --
   ---------------

   function Tuple_Con
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type
   is
   begin
      Check_Tuple (Arity);
      return Tuples.Element (Arity).Value_Con;
   end Tuple_Con;

   ----------------
   -- Tuple_Name --
   ----------------

   function Tuple_Name
     (Arity : Positive)
      return String
   is
   begin
      return S : String (1 .. Arity + 1) := (others => ',') do
         S (S'First) := '(';
         S (S'Last) := ')';
      end return;
   end Tuple_Name;

   ----------------
   -- Tuple_Type --
   ----------------

   function Tuple_Type
     (Arity : Positive)
      return Leander.Types.Trees.Tree_Type
   is
   begin
      Check_Tuple (Arity);
      return Tuples.Element (Arity).Type_Con;
   end Tuple_Type;

   ----------------
   -- World_Type --
   ----------------

   function World_Type return Leander.Types.Trees.Tree_Type is
   begin
      return Local_World_Type;
   end World_Type;

   Type_Con_0  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Leaf
                     (Leander.Kinds.Primitive);
   Type_Con_1  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_0);
   Type_Con_2  : constant Leander.Kinds.Trees.Tree_Type :=
                   Leander.Kinds.Trees.Apply
                     (Leander.Kinds.Trees.Apply
                        (Leander.Kinds.Map,
                         Type_Con_0),
                      Type_Con_1);

   Type_Variable_A : constant Leander.Types.Trees.Tree_Type :=
                       Leander.Types.Trees.Leaf
                         (Leander.Types.Variable ("a"));
begin
   Type_Variable_A.Set_Annotation (Type_Con_0);

   Local_Int_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
          ("Int"));
   Local_Int_Type.Set_Annotation (Type_Con_0);

   Local_Integer_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
          ("Integer"));
   Local_Integer_Type.Set_Annotation (Type_Con_0);

   Local_Char_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
          ("Char"));
   Local_Char_Type.Set_Annotation (Type_Con_0);

   Local_Trivial_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor ("()"));
   Local_Trivial_Type.Set_Annotation (Type_Con_0);

   Local_Trivial_Con :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor ("()"));

   Local_World_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor
          ("World#"));
   Local_World_Type.Set_Annotation (Type_Con_0);

   Local_List_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor ("[]"));
   Local_List_Type.First_Leaf.Set_Annotation (Type_Con_1);

   Local_Map_Type :=
     Leander.Types.Trees.Leaf
       (Leander.Types.Constructor ("->"));
   Local_Map_Type.First_Leaf.Set_Annotation (Type_Con_2);

   Local_Empty_List :=
     Leander.Types.Trees.Apply
       (Local_List_Type, Type_Variable_A);

   Local_Cons :=
     Leander.Types.Trees.Apply
       (Leander.Types.Trees.Apply
          (Leander.Types.Constructor ("->"),
           Leander.Types.Trees.Apply
             (Local_List_Type, Type_Variable_A)),
        Leander.Types.Trees.Apply
          (Local_List_Type, Type_Variable_A));
   Local_Cons :=
     Leander.Types.Trees.Apply
       (Leander.Types.Trees.Apply
          (Leander.Types.Constructor ("->"), Type_Variable_A),
        Local_Cons);

end Leander.Primitives;
