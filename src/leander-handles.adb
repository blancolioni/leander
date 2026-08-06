with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Streams;
with Ada.Text_IO;
with Leander.Byte_Buffers;
with Leander.Calculus;

with Leander.Core.Expressions.Inference;
with Leander.Core.Inference;
with Leander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Core.Schemes;
with Leander.Core.Schemes.Serialize;
with Leander.Core.Type_Classes;
with Leander.Core.Type_Classes.Serialize;
with Leander.Core.Type_Env;
with Leander.Core.Type_Instances;
with Leander.Core.Type_Instances.Serialize;
with Leander.Core.Types;
with Leander.Data_Types;
with Leander.Data_Types.Serialize;
with Leander.Environment.Prelude;
with Leander.Names;
with Leander.Primitives;
with Leander.Resources;
with Leander.Syntax.Expressions;

with Skit.Combinators;
with Skit.Compiler;
with Skit.Handles.Images;
with Skit.Terms;

package body Leander.Handles is

   --  Synthetic bookkeeping export names carrying module-level declarative
   --  metadata (classes, instance facts, data types, fixity) that has no
   --  export of its own to hang an Annotations-section payload off. Each is
   --  bound to a placeholder object (Skit.Combinators.I) purely so
   --  Skit.Handles.Images.Write's per-export lookup succeeds; the real
   --  payload travels as that export's annotation bytes. Namespaced per
   --  module (see issue #65) so two independently-dumped modules loaded
   --  together can't collide.

   Class_Prefix    : constant String := "class:";
   Data_Type_Prefix : constant String := "datatype:";
   Instance_Prefix : constant String := "instance:";
   Fixity_Prefix   : constant String := "fixity:";
   Meta_Prefix     : constant String := "__leander_meta__:";

   Meta_Version : constant := 1;

   function Has_Prefix (S, Prefix : String) return Boolean
   is (S'Length >= Prefix'Length
       and then S (S'First .. S'First + Prefix'Length - 1) = Prefix);

   function Img (N : Natural) return String;
   --  Natural'Image has a leading blank for the sign position; strip it so
   --  a class/data-type/instance's positional index embeds cleanly into a
   --  ':'-separated synthetic export name.

   ---------
   -- Img --
   ---------

   function Img (N : Natural) return String is
      S : constant String := Natural'Image (N);
   begin
      return S (S'First + 1 .. S'Last);
   end Img;

   function Trailing_Index (S : String) return Positive;
   --  The Positive after S's last ':' -- the positional index a
   --  class/data-type/instance synthetic export name (see Img above) was
   --  built with, regardless of what Module_Name itself contains.

   --------------------
   -- Trailing_Index --
   --------------------

   function Trailing_Index (S : String) return Positive is
   begin
      for I in reverse S'Range loop
         if S (I) = ':' then
            return Positive'Value (S (I + 1 .. S'Last));
         end if;
      end loop;
      raise Program_Error with "no ':' in " & S;
   end Trailing_Index;

   function Encode_Fixities
     (Entries : Leander.Parser.Fixity_Entry_Array)
      return Ada.Streams.Stream_Element_Array;

   function Decode_Fixities
     (Bytes : Ada.Streams.Stream_Element_Array)
      return Leander.Parser.Fixity_Entry_Array;

   ---------------------
   -- Encode_Fixities --
   ---------------------

   function Encode_Fixities
     (Entries : Leander.Parser.Fixity_Entry_Array)
      return Ada.Streams.Stream_Element_Array
   is
      use Ada.Strings.Unbounded;
      W : Leander.Byte_Buffers.Writer;
   begin
      W.Put_U32 (Entries'Length);
      for E of Entries loop
         W.Put_String (To_String (E.Operator));
         W.Put_U8 (E.Associativity);
         W.Put_U8 (E.Priority);
      end loop;
      return W.To_Bytes;
   end Encode_Fixities;

   ---------------------
   -- Decode_Fixities --
   ---------------------

   function Decode_Fixities
     (Bytes : Ada.Streams.Stream_Element_Array)
      return Leander.Parser.Fixity_Entry_Array
   is
      use Ada.Strings.Unbounded;
      package BB renames Leander.Byte_Buffers;
      C  : BB.Offset := Bytes'First;
      Cn : constant Natural := BB.Get_U32 (Bytes, C);
   begin
      return R : Leander.Parser.Fixity_Entry_Array (1 .. Cn) do
         for I in R'Range loop
            declare
               Op    : constant String := BB.Get_String (Bytes, C);
               Assoc : constant Natural := BB.Get_U8 (Bytes, C);
               Prio  : constant Natural := BB.Get_U8 (Bytes, C);
            begin
               R (I) :=
                 (Operator      => To_Unbounded_String (Op),
                  Associativity => Assoc,
                  Priority      => Prio);
            end;
         end loop;
      end return;
   end Decode_Fixities;

   procedure Evaluate_Error (H : Handle'Class);

   function Try_Load_Image
     (This          : in out Instance'Class;
      Env           : Leander.Environment.Reference;
      Source_Path   : String;
      Full_Coverage : out Boolean)
      return Boolean;
   --  If a sibling "<basename>.skix" of Source_Path exists and is not older
   --  than it, load it into This.Skit_Handle (priming Resolve's existing
   --  Skit_Handle.Lookup fast path so covered bindings never reach
   --  Get_Bound_Calculus) and apply every annotation to Env: ordinary
   --  exports' decoded Schemes (Set_Scheme), and, for a module dumped with
   --  the full declarative metadata described in issue #65, its classes,
   --  data types, instance facts, and fixity declarations too -- applied in
   --  that fixed order once every annotation has been read and bucketed by
   --  kind, regardless of the order the image happens to store them in.
   --  Full_Coverage is set True only if the image carries the version
   --  marker proving it covers all of that (not just an older values/
   --  schemes-only image): the caller uses this to decide whether the
   --  module's source can be skipped entirely, not just supplemented.
   --  Returns whether the image was used at all; on any failure (missing,
   --  stale, corrupt, an unresolved import) this returns False and leaves
   --  This/Env exactly as they were -- the caller falls back to ordinary
   --  from-source, lazy compilation, unchanged.

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This      : in out Instance'Class;
      Name      : String;
      Evaluator : Skit.Primitive_Evaluator_Interface'Class)
   is
      X : constant Skit.Object :=
            This.Skit_Handle.Primitive
              (Evaluator);
   begin
      This.Skit_Handle.Bind (Name, X);
   end Bind;

   -----------
   -- Close --
   -----------

   procedure Close (This : in out Instance'Class) is
   begin
      null;
   end Close;

   -------------
   -- Compile --
   -------------

   procedure Compile
     (This       : in out Instance'Class;
      Expression : String)
   is

      procedure Do_Compile;

      ----------------
      -- Do_Compile --
      ----------------

      procedure Do_Compile is
         use Leander.Core.Inference;
         use Leander.Core.Expressions.Inference;
         Syntax : constant Leander.Syntax.Expressions.Reference :=
                    This.Context.Parse_Expression (Expression);
         Core   : constant Leander.Core.Expressions.Reference :=
                    Syntax.To_Core;
         Result : Inference_Context :=
                    Initial_Context (This.Env.Type_Env);

      begin
         Leander.Syntax.Prune;
         Infer (Result, Core);
         if not Result.OK then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, Result.Error_Message);
         else

            Result.Update_Type (Core);

            declare
               Tree          : constant Leander.Calculus.Tree :=
                                 Core.To_Calculus (Result, This.Env);
               Term          : constant Skit.Terms.Term :=
                 Leander.Calculus.Compile (Tree);
               Compiled_Term : constant Skit.Terms.Term :=
                 Skit.Compiler.Compile (Term);

               function Resolve (Name : String) return Skit.Object
               is (This.Resolve (Name));

            begin
               This.Skit_Handle.Install
                 (Compiled_Term, Resolve'Access);
               Skit.Terms.Reset;
            end;
         end if;
      end Do_Compile;

   begin
      --  All types inferred here are transient (ADR 0001): the surviving
      --  artifact is the off-arena Calculus.Tree installed into the machine.
      Leander.Core.Types.Begin_Scratch;
      Do_Compile;
      Leander.Core.Types.End_Scratch;
   exception
      when others =>
         Leander.Core.Types.End_Scratch;
         raise;
   end Compile;

   ------------
   -- Create --
   ------------

   function Create
     (Size      : Natural;
      User_Data : access Leander.User_Data_Interface'Class)
      return Reference
   is
      Context  : constant Context_Reference :=
                   new Leander.Parser.Parse_Context;
      Prelude_Path : constant String :=
                       Leander.Resources.Resource_Path
                       & "/modules/Prelude.hs";
      Env          : Leander.Environment.Reference :=
                   Leander.Environment.Prelude.Create;
      This : constant Reference :=
               new Instance'
                     (Skit_Handle => <>,
                      Env         => Env,
                      Context     => Context,
                      User_Data   => User_Data_Reference (User_Data),
                      IO          => Leander.IO.Local_IO,
                      Slots       => <>);
      Full_Coverage : Boolean;
   begin
      This.Skit_Handle :=
        Skit.Handles.New_Handle
          (Core_Size => Size,
           User_Data => This);
      Leander.Primitives.Load_Primitives (This.Skit_Handle);

      This.Bind
        ("#error",
         Binding_Instance'
           (Argument_Count => 1,
            Result_Count   => 1,
            Arg_Types      => [String_Type],
            Res_Types      => [Boolean_Type],
            Eval           => Evaluate_Error'Access));

      --  Attempt a full offline load first: Env starts out as the hand-built
      --  Prelude scaffold (the builtin ()/(,)/Bool/[] types -- Prelude.hs's
      --  own source never declares these; see
      --  Leander.Environment.Prelude.Create), and a complete, fresh sibling
      --  .skix supplies everything else (classes, instances, data types,
      --  fixity, and every compiled value) without ever opening Prelude.hs.
      --  Registering the result lets a later Load_Module ("Prelude") --
      --  e.g. from a downstream module's own import -- reuse it too,
      --  instead of re-parsing. Anything short of complete coverage (no
      --  image, a stale one, or an older values/schemes-only image) falls
      --  back to the ordinary full source parse and today's supplement-only
      --  priming, unchanged.
      if This.Try_Load_Image (Env, Prelude_Path, Full_Coverage)
        and then Full_Coverage
      then
         Context.Register_Loaded_Module ("Prelude", Env);
      else
         Env := Context.Load_Module (Prelude_Path);
         This.Env := Env;
         if This.Try_Load_Image (Env, Prelude_Path, Full_Coverage) then
            null;
         end if;
      end if;

      return This;
   end Create;

   -------------------------
   -- Current_Environment --
   -------------------------

   function Current_Environment
     (This : Instance'Class)
      return String
   is
   begin
      return This.Env.Name;
   end Current_Environment;

   -----------------
   -- Dump_Module --
   -----------------

   procedure Dump_Module
     (This        : in out Instance'Class;
      Path        : String;
      Module_Name : String := "module")
   is
      use Ada.Strings.Unbounded;
      use type Leander.Core.Type_Env.Nullable_Scheme_Reference;

      Ids       : constant Leander.Names.Name_Array := This.Env.Value_Names;
      Classes   : constant Leander.Environment.Type_Class_Array :=
                    This.Env.Own_Classes;
      Data_Types_List : constant Leander.Environment.Data_Type_Array :=
                    This.Env.Own_Data_Types;
      Instances : constant Leander.Core.Type_Instances.Reference_Array :=
                    This.Env.Own_Instances;

      Extra_Count : constant Natural :=
        Classes'Length + Data_Types_List'Length + Instances'Length + 2;

      Exports : Skit.Handles.Images.Name_Array (1 .. Ids'Length + Extra_Count);
      Count   : Natural := 0;

      Fixity_Name : constant String := Fixity_Prefix & Module_Name;
      Meta_Name   : constant String := Meta_Prefix & Module_Name;

      function Annotation_Of (Export_Name : String)
        return Ada.Streams.Stream_Element_Array;

      -------------------
      -- Annotation_Of --
      -------------------

      function Annotation_Of (Export_Name : String)
        return Ada.Streams.Stream_Element_Array
      is
      begin
         if Export_Name = Meta_Name then
            return Bytes : Ada.Streams.Stream_Element_Array (1 .. 1) do
               Bytes (1) := Ada.Streams.Stream_Element (Meta_Version);
            end return;
         elsif Export_Name = Fixity_Name then
            return Encode_Fixities (Leander.Parser.All_Fixities);
         elsif Has_Prefix (Export_Name, Class_Prefix) then
            return Leander.Core.Type_Classes.Serialize.Encode
              (Classes (Trailing_Index (Export_Name)));
         elsif Has_Prefix (Export_Name, Data_Type_Prefix) then
            return Leander.Data_Types.Serialize.Encode
              (Data_Types_List (Trailing_Index (Export_Name)));
         elsif Has_Prefix (Export_Name, Instance_Prefix) then
            return Leander.Core.Type_Instances.Serialize.Encode
              (Instances (Trailing_Index (Export_Name)));
         else
            declare
               Scheme : constant
                 Leander.Core.Type_Env.Nullable_Scheme_Reference :=
                    This.Env.Type_Env.Lookup
                      (Leander.Names.To_Leander_Name (Export_Name));
            begin
               if Scheme = null then
                  return Ada.Streams.Stream_Element_Array'
                    (1 .. 0 => <>);
               else
                  return Leander.Core.Schemes.Serialize.Encode
                    (Leander.Core.Schemes.Reference (Scheme));
               end if;
            end;
         end if;
      end Annotation_Of;

   begin
      --  Binding_Groups.Varids (behind Value_Names) over-approximates: it
      --  also surfaces clause-pattern variables from equation desugaring
      --  (e.g. the "xs" in "map f (x:xs) = ..."), which have no top-level
      --  value of their own. Variable_Binding_Exists is the same guard
      --  Resolve itself checks before raising, so filtering on it here
      --  keeps exactly the names Resolve can actually compile.
      for Id of Ids loop
         declare
            Name : constant String := Leander.Names.To_String (Id);
         begin
            if This.Env.Variable_Binding_Exists (Name) then
               declare
                  Value : Skit.Object;
               begin
                  --  Resolve is called as a statement, not a declaration
                  --  initializer: an exception raised while elaborating a
                  --  block's declarative part is NOT caught by that same
                  --  block's own handler, only by an enclosing one.
                  Value := This.Resolve (Name);

                  --  A name bound directly to a bare primitive function
                  --  (a "foreign import ... #name" wrapper) has no export
                  --  representation: Put_Object (unlike Put_Slot) cannot
                  --  emit it as a named import, and nothing needs to look
                  --  it up post-load anyway -- any Prelude function that
                  --  uses it embeds it correctly via a cell slot already.
                  --  Resolve has still bound it into the handle, so those
                  --  embedded references resolve.
                  if not Skit.Is_Primitive_Function (Value) then
                     Count := Count + 1;
                     Exports (Count) := To_Unbounded_String (Name);
                  end if;
               exception
                  --  A binding that fails to compile (e.g. a latent gap in
                  --  recursive-let compilation never exercised by the
                  --  ordinary REPL/test path, which only forces what a
                  --  given expression actually reaches) is demoted to a
                  --  skipped export, not a fatal dump failure -- mirroring
                  --  ADR 0002's "Serialize may fail; the referencing
                  --  binding is demoted to an error, the rest proceeds."
                  when others =>
                     Ada.Text_IO.Put_Line
                       (Ada.Text_IO.Standard_Error,
                        "Dump_Module: skipping """ & Name
                        & """, failed to compile");
               end;
            end if;
         end;
      end loop;

      --  Synthetic bookkeeping exports carrying this module's own
      --  declarative metadata (classes, data types, instance facts,
      --  fixity), namespaced per module, plus a version marker Try_Load_
      --  Image uses to tell a fully-covering image apart from an older
      --  values/schemes-only one -- see the Class_Prefix &c. comment above.
      for I in Classes'Range loop
         declare
            Name : constant String := Class_Prefix & Module_Name & ":" & Img (I);
         begin
            This.Skit_Handle.Bind (Name, Skit.Combinators.I);
            Count := Count + 1;
            Exports (Count) := To_Unbounded_String (Name);
         end;
      end loop;

      for I in Data_Types_List'Range loop
         declare
            Name : constant String :=
              Data_Type_Prefix & Module_Name & ":" & Img (I);
         begin
            This.Skit_Handle.Bind (Name, Skit.Combinators.I);
            Count := Count + 1;
            Exports (Count) := To_Unbounded_String (Name);
         end;
      end loop;

      for I in Instances'Range loop
         declare
            Name : constant String :=
              Instance_Prefix & Module_Name & ":" & Img (I);
         begin
            This.Skit_Handle.Bind (Name, Skit.Combinators.I);
            Count := Count + 1;
            Exports (Count) := To_Unbounded_String (Name);
         end;
      end loop;

      This.Skit_Handle.Bind (Fixity_Name, Skit.Combinators.I);
      Count := Count + 1;
      Exports (Count) := To_Unbounded_String (Fixity_Name);

      This.Skit_Handle.Bind (Meta_Name, Skit.Combinators.I);
      Count := Count + 1;
      Exports (Count) := To_Unbounded_String (Meta_Name);

      Skit.Handles.Images.Write
        (This.Skit_Handle, Path, Exports (1 .. Count), Module_Name,
         Annotation_Of'Access);
   end Dump_Module;

   --------------
   -- Evaluate --
   --------------

   procedure Evaluate
     (This       : in out Instance'Class;
      Expression : String)
   is
   begin
      This.Compile (Expression);
      This.Skit_Handle.Evaluate;
   end Evaluate;

   --------------------
   -- Evaluate_Error --
   --------------------

   procedure Evaluate_Error (H : Handle'Class) is
      Message : constant String :=
        H.Get_Slot (1);
   begin
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error, Message);
      raise Constraint_Error with Message;
   end Evaluate_Error;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (This : Instance'Class;
      Slot : Slot_Index)
      return Boolean
   is
   begin
      return This.Slots (Slot).Boolean_Value;
   end Get_Slot;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (This : Instance'Class;
      Slot : Slot_Index)
      return String
   is
   begin
      return Ada.Strings.Unbounded.to_string
        (This.Slots (Slot).String_Value);
   end Get_Slot;

   --------------
   -- Get_Slot --
   --------------

   function Get_Slot
     (This : Instance'Class;
      Slot : Slot_Index)
      return Integer
   is
   begin
      return This.Slots (Slot).Integer_Value;
   end Get_Slot;

   ----------------
   -- Infer_Type --
   ----------------

   function Infer_Type
     (This       : in out Instance'Class;
      Expression : String)
      return String
   is

      function Do_Infer_Type return String;

      -------------------
      -- Do_Infer_Type --
      -------------------

      function Do_Infer_Type return String is
         use Leander.Core.Inference;
         use Leander.Core.Expressions.Inference;
         Syntax : constant Leander.Syntax.Expressions.Reference :=
                    This.Context.Parse_Expression (Expression);
         Core   : constant Leander.Core.Expressions.Reference :=
                    Syntax.To_Core;
         Result : Inference_Context :=
                    Initial_Context (This.Env.Type_Env);
      begin
         Infer (Result, Core);
         if not Result.OK then
            return Result.Error_Message;
         else
            declare
               Ps      : constant Leander.Core.Predicates.Predicate_Array :=
                           Result.Current_Predicates;
               Success : Boolean;
               Reduced : constant Leander.Core.Predicates.Predicate_Array :=
                           Leander.Core.Type_Classes.Class_Environment'Class
                             (This.Env.all).Reduce (Ps, Success);
            begin
               return Leander.Core.Qualified_Types.Qualified_Type
                 ((if Success then Reduced else Ps),
                  Result.Get_Type (Core)).Generate.Show;
            end;
         end if;
      end Do_Infer_Type;

   begin
      --  Transient types only; the result is a formatted string (ADR 0001).
      Leander.Core.Types.Begin_Scratch;
      return Result : constant String := Do_Infer_Type do
         Leander.Core.Types.End_Scratch;
      end return;
   exception
      when others =>
         Leander.Core.Types.End_Scratch;
         raise;
   end Infer_Type;

   -----------------
   -- Load_Module --
   -----------------

   procedure Load_Module
     (This : in out Instance'Class;
      Path : String)
   is
   begin
      This.Env := This.Context.Load_Module (Path);
   end Load_Module;

   ---------
   -- Pop --
   ---------

   function Pop
     (This : Instance'Class)
      return String
   is
   begin
      return This.Skit_Handle.Image
        (This.Skit_Handle.Pop);
   end Pop;

   -------------------
   -- Receive_Value --
   -------------------

   function Receive_Value
     (This   : Instance'Class;
      Index  : Slot_Index)
      return Skit.Object
   is
      Value : constant Foreign_Value := This.Slots (Index);
      H     : constant Skit.Handles.Handle := This.Skit_Handle;
   begin
      case Value.Class is
         when Unit_Type =>
            return Skit.Combinators.I;
         when Boolean_Type =>
            if Value.Boolean_Value then
               return Skit.Combinators.K;
            else
               H.Push (Skit.Combinators.K);
               H.Push (Skit.Combinators.I);
               H.Apply;
               return H.Pop;
            end if;
         when Integer_Type =>
            return Skit.To_Object (Value.Integer_Value);
         when String_Type =>
            declare
               use Ada.Strings.Unbounded;
               S  : constant String :=
                      To_String (Value.String_Value);
            begin
               for Ch of S loop
                  H.Push (H.Lookup ("#cons"));
                  H.Push (Skit.To_Object (Character'Pos (Ch)));
                  H.Apply;
               end loop;
               H.Push (SKit.Combinators.K);
               for I in S'Range loop
                  H.Apply;
               end loop;
               return H.Pop;
            end;
      end case;
   end Receive_Value;

   ------------
   -- Report --
   ------------

   procedure Report
     (This : in out Instance'Class)
   is
   begin
      This.Skit_Handle.Report;
      if False then
         Leander.Core.Report;
         Leander.Syntax.Report;
      end if;
   end Report;

   -------------
   -- Resolve --
   -------------

   function Resolve
     (This : Instance'Class;
      Name : String)
      return Skit.Object
   is
      Binding : constant Skit.Object := This.Skit_Handle.Lookup (Name);
   begin
      if Skit.Is_Undefined (Binding) then
         if not This.Env.Variable_Binding_Exists (Name) then
            raise Program_Error with
              "undefined: " & Name;
         end if;

         declare
            T             : constant Leander.Calculus.Tree :=
                              This.Env.Get_Bound_Calculus (Name);
            Term          : constant Skit.Terms.Term :=
                              Leander.Calculus.Compile (T);
            Compiled_Term : constant Skit.Terms.Term :=
                              Skit.Compiler.Compile (Term);
            function Resolve (Name : String) return Skit.Object
            is (This.Resolve (Name));

            Value         : constant Skit.Object :=
                              This.Skit_Handle.Install
                                (Compiled_Term, Resolve'Access);
         begin
            This.Skit_Handle.Bind (Name, Value);
            return Value;
         end;
      else
         return Binding;
      end if;
   end Resolve;

   ----------------
   -- Send_Value --
   ----------------

   procedure Send_Value
     (This   : in out Instance'Class;
      Index  : Slot_Index;
      F_Type : Foreign_Type;
      Value  : Skit.Object)
   is
      use type Skit.Object;
      F_Value : Foreign_Value;
   begin
      case F_Type.Class is
         when Unit_Type =>
            F_Value := (Class => Unit_Type);
         when Boolean_Type =>
            F_Value := (Boolean_Type, Value = Skit.Combinators.K);
         when Integer_Type =>
            F_Value := (Integer_Type, Skit.To_Integer (Value));
         when String_Type =>
            declare
               use Ada.Strings.Unbounded;
               It : Skit.Object := Value;
               S  : Unbounded_String;
            begin
               while It /= Skit.Combinators.K loop
                  declare
                     Code : constant Skit.Object :=
                              This.Skit_Handle.Right
                                (This.Skit_Handle.Left (It));
                  begin
                     Append (S, Character'Val (Skit.To_Integer (Code)));
                     It := This.Skit_Handle.Right (It);
                  end;
               end loop;
               F_Value := (String_Type, S);
            end;
      end case;
      This.Slots (Index) := F_Value;
   end Send_Value;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : in out Instance'Class;
      Slot  : Slot_Index;
      Value : Boolean)
   is
   begin
      This.Slots (Slot) := (Boolean_Type, Value);
   end Set_Slot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : in out Instance'Class;
      Slot  : Slot_Index;
      Value : String)
   is
   begin
      This.Slots (Slot) :=
        (String_Type, Ada.Strings.Unbounded.To_Unbounded_String (Value));
   end Set_Slot;

   --------------
   -- Set_Slot --
   --------------

   procedure Set_Slot
     (This  : in out Instance'Class;
      Slot  : Slot_Index;
      Value : Integer)
   is
   begin
      This.Slots (Slot) := (Integer_Type, Value);
   end Set_Slot;

   ---------------------
   -- Try_Load_Image --
   ---------------------

   function Try_Load_Image
     (This          : in out Instance'Class;
      Env           : Leander.Environment.Reference;
      Source_Path   : String;
      Full_Coverage : out Boolean)
      return Boolean
   is
      use type Ada.Calendar.Time;

      Image_Path : constant String :=
                     Ada.Directories.Compose
                       (Ada.Directories.Containing_Directory
                          (Ada.Directories.Full_Name (Source_Path)),
                        Ada.Directories.Base_Name (Source_Path),
                        "skix");

      package Data_Type_Vectors is new Ada.Containers.Vectors
        (Positive, Leander.Data_Types.Reference, Leander.Data_Types."=");
      package Type_Class_Vectors is new Ada.Containers.Vectors
        (Positive, Leander.Core.Type_Classes.Reference,
         Leander.Core.Type_Classes."=");
      package Type_Instance_Vectors is new Ada.Containers.Vectors
        (Positive, Leander.Core.Type_Instances.Reference,
         Leander.Core.Type_Instances."=");

      --  Annotations arrive in whatever order the image happens to store
      --  them in (not necessarily Dump_Module's write order -- see
      --  skit-handles-images.adb, which sorts them by export name), so data
      --  types/classes/instances are buffered here and applied in the fixed
      --  order Environment.Elaborate itself requires (data types, then
      --  classes, then instances) only after Read completes. Fixity has no
      --  such dependency and is applied immediately.
      Pending_Data_Types : Data_Type_Vectors.Vector;
      Pending_Classes    : Type_Class_Vectors.Vector;
      Pending_Instances  : Type_Instance_Vectors.Vector;

      procedure On_Annotation
        (Export_Name : String;
         Bytes       : Ada.Streams.Stream_Element_Array);

      -------------------
      -- On_Annotation --
      -------------------

      procedure On_Annotation
        (Export_Name : String;
         Bytes       : Ada.Streams.Stream_Element_Array)
      is
      begin
         if Has_Prefix (Export_Name, Meta_Prefix) then
            Full_Coverage := True;
         elsif Has_Prefix (Export_Name, Class_Prefix) then
            Pending_Classes.Append
              (Leander.Core.Type_Classes.Serialize.Decode (Bytes));
         elsif Has_Prefix (Export_Name, Data_Type_Prefix) then
            Pending_Data_Types.Append
              (Leander.Data_Types.Serialize.Decode (Bytes));
         elsif Has_Prefix (Export_Name, Instance_Prefix) then
            Pending_Instances.Append
              (Leander.Core.Type_Instances.Serialize.Decode (Bytes));
         elsif Has_Prefix (Export_Name, Fixity_Prefix) then
            for E of Decode_Fixities (Bytes) loop
               Leander.Parser.Add_Fixity
                 (Ada.Strings.Unbounded.To_String (E.Operator),
                  E.Associativity, E.Priority);
            end loop;
         else
            Env.Set_Scheme
              (Export_Name, Leander.Core.Schemes.Serialize.Decode (Bytes));
         end if;
      end On_Annotation;

   begin
      Full_Coverage := False;

      if not Ada.Directories.Exists (Image_Path)
        or else Ada.Directories.Modification_Time (Image_Path)
                  < Ada.Directories.Modification_Time (Source_Path)
      then
         return False;
      end if;

      Skit.Handles.Images.Read
        (This.Skit_Handle, Image_Path, On_Annotation'Access);

      --  A module's environment can arrive here already partly seeded --
      --  notably Leander.Environment.Prelude.Create's hand-built ()/(,)/
      --  Bool/[] scaffold, which Own_Data_Types (see Dump_Module) exports
      --  right alongside Prelude.hs's own data types -- so re-registering
      --  an already-present class/data type is expected, not an error, and
      --  must be skipped rather than raising on the map's duplicate-key
      --  check.
      for DT of Pending_Data_Types loop
         if not Env.Exists
           (Leander.Names.Leander_Name (DT.Id),
            Leander.Environment.Type_Constructor)
         then
            Env.Data_Type (DT);
         end if;
      end loop;

      for C of Pending_Classes loop
         if not Env.Exists
           (Leander.Names.Leander_Name (C.Id), Leander.Environment.Class_Binding)
         then
            Env.Type_Class (C);
         end if;
      end loop;

      for Inst of Pending_Instances loop
         Env.Type_Instance
           (Class_Id      => Inst.Predicate.Class_Id,
            Constraints   => Inst.Qualifier.Predicates,
            Instance_Type => Inst.Predicate.Get_Type,
            Bindings      => null);
      end loop;

      return True;
   exception
      when others =>
         Full_Coverage := False;
         return False;
   end Try_Load_Image;

end Leander.Handles;
