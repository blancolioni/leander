with Ada.Text_IO;

with Leander.Core.Types.Unification;
with Leander.Names;
with Leander.Traverseable;

package body Leander.Core.Inference is

   -----------------------
   -- Add_Error_Context --
   -----------------------

   procedure Add_Error_Context
     (This    : in out Inference_Context'Class;
      Context : String)
   is
      use Leander.Names;
   begin
      if This.OK then
         This.Error (Context);
      else
         This.Error_Message :=
           Leander.Names.To_Leander_Name
             (To_String (This.Error_Message) & Character'Val (10)
              & Context);
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            Context);
      end if;
   end Add_Error_Context;

   ----------
   -- Bind --
   ----------

   procedure Bind
     (This  : in out Inference_Context'Class;
      Item  : not null access constant Leander.Core.Typeable.Abstraction'Class;
      To    : Leander.Core.Types.Reference)
   is
   begin
      This.Expr_Types.Insert (Item, Nullable_Type_Reference (To));
   end Bind;

   -------------
   -- Binding --
   -------------

   function Binding
     (This : Inference_Context;
      Item : not null access constant Leander.Core.Typeable.Abstraction'Class)
      return Core.Types.Reference
   is
     begin
      if This.Expr_Types.Contains (Item) then
         return Leander.Core.Types.Reference
           (This.Expr_Types.Element (Item));
      else
         raise Constraint_Error
           with "no binding for expression: " & Item.Show;
      end if;
   end Binding;

   ----------------------
   -- Clear_Predicates --
   ----------------------

   procedure Clear_Predicates
     (This : in out Inference_Context)
   is
   begin
      This.Predicates.Clear;
   end Clear_Predicates;

   ---------------------
   -- Drop_Predicates --
   ---------------------

   procedure Drop_Predicates
     (This : in out Inference_Context;
      From : Positive)
   is
   begin
      while Natural (This.Predicates.Length) >= From loop
         This.Predicates.Delete_Last;
      end loop;
   end Drop_Predicates;

   ---------------------
   -- Predicate_Count --
   ---------------------

   function Predicate_Count
     (This : Inference_Context)
      return Natural
   is (Natural (This.Predicates.Length));

   ------------------------
   -- Current_Predicates --
   ------------------------

   function Current_Predicates
     (This : Inference_Context)
      return Leander.Core.Predicates.Predicate_Array
   is
      function Apply_Subst
        (T : Leander.Core.Types.Reference)
         return Leander.Core.Types.Reference
      is (Leander.Core.Types.Reference (T.Apply (This.Subst)));
   begin
      return [for P of This.Predicates =>
               Core.Predicates.Predicate
                  (Predicates.Class_Name (P),
                   Apply_Subst (Predicates.Get_Type (P)))];
   end Current_Predicates;

   ---------------------
   -- Raw_Predicates --
   ---------------------

   function Raw_Predicates
     (This : Inference_Context)
      return Leander.Core.Predicates.Predicate_Array
   is ([for P of This.Predicates => P]);

   --------------------------
   -- Current_Substitution --
   --------------------------

   function Current_Substitution
     (This : Inference_Context)
      return Leander.Core.Substitutions.Instance
   is
   begin
      return This.Subst;
   end Current_Substitution;

   -----------
   -- Error --
   -----------

   procedure Error
     (This  : in out Inference_Context'Class;
      Message : String)
   is
   begin
      This.Success := False;
      This.Error_Message := Leander.Names.To_Leander_Name (Message);
      Ada.Text_IO.Put_Line
        (Ada.Text_IO.Standard_Error,
         Message);
   end Error;

   ----------------------
   -- Restore_Type_Env --
   ----------------------

   procedure Restore_Type_Env (This : in out Inference_Context) is
   begin
      This.Type_Env := This.Env_Stack.Last_Element;
      This.Env_Stack.Delete_Last;
   end Restore_Type_Env;

   ---------------------
   -- Save_Predicates --
   ---------------------

   procedure Save_Predicates
     (This       : in out Inference_Context;
      Predicates : Leander.Core.Predicates.Predicate_Array)
   is
   begin
      for P of Predicates loop
         This.Predicates.Append (P);
      end loop;
   end Save_Predicates;

   -----------------------
   -- Save_Substitution --
   -----------------------

   procedure Save_Substitution
     (This  : in out Inference_Context;
      Subst : Leander.Core.Substitutions.Instance'Class)
   is
      procedure Save
        (Name : Leander.Names.Leander_Name;
         Ty   : not null access constant Leander.Core.Types.Instance'Class);

      ----------
      -- Save --
      ----------

      procedure Save
        (Name : Leander.Names.Leander_Name;
         Ty   : not null access constant Leander.Core.Types.Instance'Class)
      is
         use type Leander.Core.Substitutions.Nullable_Type_Reference;
         Bound : constant Leander.Core.Substitutions.Nullable_Type_Reference :=
                   This.Subst.Lookup (Name);
      begin
         if Subst.Lookup (Name) /= Ty then
            --  A later binding of a name Subst binds earlier: composition
            --  eliminated Name before this was reached, so it says nothing.
            null;
         elsif Bound = null then
            --  Resolved against the context first, so that substituting it
            --  into the context's own bindings cannot bring back a variable
            --  the context binds: every binding has to resolve in one step,
            --  since Apply looks each variable up only once.
            declare
               Resolved : constant Leander.Core.Types.Reference :=
                            Leander.Core.Types.Reference
                              (Ty.Apply (This.Subst));
            begin
               if not Resolved.Is_Variable
                 or else Resolved.Variable.Name /= Varid (Name)
               then
                  This.Subst :=
                    Leander.Core.Substitutions.Compose
                      (Name, Resolved, This.Subst);
               end if;
            end;
         else
            --  Both bind Name.  Composing would keep only one binding and
            --  silently drop what the other says, so solve the two
            --  together instead: Name is both, so they are equal.
            This.Subst :=
              Leander.Core.Types.Unification.Most_General_Unifier
                (Bound.Apply (This.Subst), Ty.Apply (This.Subst))
              .Compose (This.Subst);
         end if;
      end Save;

   begin
      --  A substitution arrives here from an expression inferred on its own
      --  (Algorithm W style), and can bind a type variable that the context
      --  has meanwhile bound too -- as a let or case alternative nested
      --  inside that expression does, unifying straight into the context.
      --  Saving it binding by binding treats each binding as an equation
      --  to solve against the context rather than as a replacement for it
      --  (issue #93).
      Subst.Iterate (Save'Access);
   end Save_Substitution;

   -------------------
   -- Save_Type_Env --
   -------------------

   procedure Save_Type_Env (This : in out Inference_Context) is
   begin
      This.Env_Stack.Append (This.Type_Env);
   end Save_Type_Env;

   -------------------
   -- Save_Type_Env --
   -------------------

   procedure Save_Type_Env
     (This    : in out Inference_Context;
      New_Env : Leander.Core.Type_Env.Reference)
   is
   begin
      This.Save_Type_Env;
      This.Type_Env := New_Env;
   end Save_Type_Env;

   ----------------
   -- Set_Result --
   ----------------

   procedure Set_Result
     (This  : in out Inference_Context'Class;
      Ty    : Leander.Core.Types.Reference)
   is
   begin
      This.Inferred_Type := Nullable_Type_Reference (Ty);
   end Set_Result;

   -----------------
   -- Update_Type --
   -----------------

   procedure Update_Type
     (This  : Inference_Context;
      Root  : not null access
        Leander.Core.Qualified_Types.Has_Qualified_Type'Class)
   is
      procedure Update
        (Traversable : not null access Leander.Traverseable.Abstraction'Class);

      ------------
      -- Update --
      ------------

      procedure Update
        (Traversable : not null access Leander.Traverseable.Abstraction'Class)
      is
         use Leander.Core.Qualified_Types;
         HQT : Has_Qualified_Type'Class renames
                 Has_Qualified_Type'Class (Traversable.all);
      begin
         if HQT.Has_Qualified_Type_Value then
            declare
               QT  : constant Leander.Core.Qualified_Types.Reference :=
                       HQT.Qualified_Type.all.Apply (This.Current_Substitution);
            begin
               HQT.Set_Qualified_Type (QT);
            end;
         end if;
      end Update;

   begin
      Root.Update_Traverse (Update'Access);
   end Update_Type;

   ---------------------
   -- Update_Type_Env --
   ---------------------

   procedure Update_Type_Env
     (This : in out Inference_Context;
      Env  : Leander.Core.Type_Env.Reference)
   is
   begin
      This.Type_Env := Env;
   end Update_Type_Env;

end Leander.Core.inference;
