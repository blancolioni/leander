with Ada.Text_IO;
with Leander.Handles;
with Skit.Compiler;
with Skit.Parser;
with Skit.Terms;

package body Leander.Primitives is

   use Skit;

   type Integer_Function is (Add, Sub, Mul, Divide, Modulus, Leq);

   type Integer_Evaluator (Fn : Integer_Function) is
     new Primitive_Evaluator_Interface with
       record
         null;
       end record;

   overriding function Argument_Count
     (This : Integer_Evaluator)
      return Natural
   is (2);

   overriding function Argument_Modes
     (This : Integer_Evaluator)
      return Argument_Mode_Array
   is ([Strict, Strict]);

   overriding function Evaluate
     (This      : Integer_Evaluator;
      User_Data : access Skit.User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object;

   type General_Evaluator_Fn is access
     function (Handle : Leander.Handles.Reference;
               Arguments : Object_Array)
               return Object;

   type Evaluator (Arg_Count : Natural) is
     new Primitive_Evaluator_Interface with
      record
         Modes : Argument_Mode_Array (1 .. Arg_Count);
         Fn    : General_Evaluator_Fn;
      end record;

   overriding function Argument_Count
     (This : Evaluator)
      return Natural
   is (This.Arg_Count);

   overriding function Argument_Modes
     (This : Evaluator)
      return Argument_Mode_Array
   is (This.Modes);

   overriding function Evaluate
     (This      : Evaluator;
      User_Data : access Skit.User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object;

   pragma Warnings (Off);
   function Evaluate_Equal
     (Handle : Leander.Handles.Reference;
      Arguments : Object_Array)
      return Object
   is (To_Object
       (if Arguments (1) = Arguments (2)
          then 1 else 0));

   function Evaluate_Choose
     (Handle : Leander.Handles.Reference;
      Arguments : Object_Array) return Object
   is (if Arguments (1) = To_Object (0)
       then Arguments (2)
       else Arguments (3));

   function Evaluate_Seq
     (Handle : Leander.Handles.Reference;
      Arguments : Object_Array) return Object
   is (Arguments (2));

   function Evaluate_Putchar
     (Handle : Leander.Handles.Reference;
      Arguments : Object_Array) return Object;

   function Evaluate_Trace
     (Handle : Leander.Handles.Reference;
      Arguments : Object_Array) return Object;

   pragma Warnings (On);

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Integer_Evaluator;
      User_Data : access Skit.User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is
      X : constant Integer := To_Integer (Arguments (1));
      Y : constant Integer := To_Integer (Arguments (2));
      Z : constant Integer :=
            (case This.Fn is
                when Add => X + Y,
                when Sub => X - Y,
                when Mul => X * Y,
                when Divide => X / Y,
                when Modulus => X mod Y,
                when Leq => (if X <= Y then 1 else 0));
   begin
      return To_Object (Z);
   end Evaluate;

   --------------
   -- Evaluate --
   --------------

   overriding function Evaluate
     (This      : Evaluator;
      User_Data : access Skit.User_Data_Interface'Class;
      Arguments : Object_Array)
      return Object
   is
   begin
      return This.Fn (Leander.Handles.Reference (User_Data),
                      Arguments);
   end Evaluate;

   ----------------------
   -- Evaluate_Putchar --
   ----------------------

   function Evaluate_Putchar
     (Handle : Leander.Handles.Reference;
      Arguments : Object_Array)
      return Object
   is
      File : constant Natural := To_Integer (Arguments (2));
      Code : constant Natural := To_Integer (Arguments (3));
   begin
      Handle.IO.Put (File, Wide_Wide_Character'Val (Code));
      return To_Object (To_Integer (Arguments (1)) + 1);
   end Evaluate_Putchar;

   --------------------
   -- Evaluate_Trace --
   --------------------

   function Evaluate_Trace
     (Handle : Leander.Handles.Reference;
      Arguments : Object_Array)
      return Object
   is
   begin
      Ada.Text_IO.Put_Line ("trace: " &  Handle.Debug_Image (Arguments (1)));
      return Arguments (1);
   end Evaluate_Trace;

   ---------------------
   -- Load_Primitives --
   ---------------------

   procedure Load_Primitives
     (Handle : Skit.Handles.Handle)
   is
      procedure Bind
        (Name : String;
         Value : Skit.Terms.Term);

      procedure Exec (Expr : String);

      function Resolve
        (Name : String)
         return Skit.Object;

      ----------
      -- Bind --
      ----------

      procedure Bind
        (Name : String;
         Value : Skit.Terms.Term)
      is
      begin
         Handle.Bind
           (Name => Name,
            Value =>
              Handle.Install
                (Skit.Compiler.Compile (Value),
                 Resolve'Access));
      end Bind;

      ----------
      -- Exec --
      ----------

      procedure Exec (Expr : String) is
         Unused : constant Skit.Terms.Term :=
           Skit.Parser.Parse (Expr, Bind'Access);
      begin
         pragma Unreferenced (Unused);
      end Exec;

      -------------
      -- Resolve --
      -------------

      function Resolve
        (Name : String)
         return Skit.Object
      is
      begin
         return Handle.Lookup (Name);
      end Resolve;

   begin
      Exec ("!Y S S I (C B (S I I))");
      Exec ("!#id \x.x");
      Exec ("!#false \x.\y.y");
      Exec ("!#true \x.\y.x");

      Handle.Bind ("#choose",
                   Handle.Primitive
                     (Evaluator'
                        (3, [Strict, Lazy, Lazy],
                         Evaluate_Choose'Access)));

      Handle.Bind ("#seq",
                   Handle.Primitive
                     (Evaluator'
                        (2, [Strict, Lazy],
                         Evaluate_Seq'Access)));

      Handle.Bind
        ("#equal?",
         Handle.Primitive
           (Evaluator'(2, [Strict, Strict], Evaluate_Equal'Access)));

      Exec ("!#eq \x.\y.#choose (#equal? x y) #false #true");
      Handle.Bind ("#leq?",
                   Handle.Primitive (Integer_Evaluator'(Fn => Leq)));
      Exec ("!#le \x.\y.#choose (#leq? x y) #false #true");

      Handle.Bind ("#add",
                   Handle.Primitive (Integer_Evaluator'(Fn => Add)));
      Handle.Bind ("#sub",
                   Handle.Primitive (Integer_Evaluator'(Fn => Sub)));
      Handle.Bind ("#mul",
                   Handle.Primitive (Integer_Evaluator'(Fn => Mul)));
      Handle.Bind ("#div",
                   Handle.Primitive (Integer_Evaluator'(Fn => Divide)));
      Handle.Bind ("#mod",
                   Handle.Primitive (Integer_Evaluator'(Fn => Modulus)));

      Handle.Bind ("#putChar",
                   Handle.Primitive
                     (Evaluator'
                        (3, [Strict, Strict, Strict],
                         Evaluate_Putchar'Access)));

      Handle.Bind ("#trace",
                   Handle.Primitive
                     (Evaluator'
                        (1, [Strict],
                         Evaluate_Trace'Access)));

      Handle.Bind ("#maxInt", To_Object (Max_Integer));
      Handle.Bind ("#minInt", To_Object (Min_Integer));

   end Load_Primitives;

end Leander.Primitives;
