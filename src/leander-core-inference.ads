private with Ada.Containers.Doubly_Linked_Lists;
with LEander.Core.Predicates;
with Leander.Core.Qualified_Types;
with Leander.Core.Substitutions;
with Leander.Core.Type_Env;
private with Leander.Core.Typeable.Maps;
with Leander.Core.Typeable;
with Leander.Core.Types;

package Leander.Core.Inference is

   type Inference_Context is tagged private;

   function Initial_Context
     (Type_Env : Leander.Core.Type_Env.Reference)
      return Inference_Context;

   procedure Save_Type_Env
     (This : in out Inference_Context);

   procedure Save_Type_Env
     (This    : in out Inference_Context;
      New_Env : Leander.Core.Type_Env.Reference);

   procedure Restore_Type_Env
     (This : in out Inference_Context);

   procedure Update_Type_Env
     (This : in out Inference_Context;
      Env  : Leander.Core.Type_Env.Reference);

   function Current_Substitution
     (This : Inference_Context)
      return Leander.Core.Substitutions.Instance;

   procedure Save_Substitution
     (This  : in out Inference_Context;
      Subst : Leander.Core.Substitutions.Instance'Class);

   procedure Save_Predicates
     (This : in out Inference_Context;
      Predicates : Leander.Core.Predicates.Predicate_Array);

   function Current_Predicates
     (This : Inference_Context)
      return Leander.Core.Predicates.Predicate_Array;

   function OK (This : Inference_Context) return Boolean;
   function Error_Message (This : Inference_Context) return String;

   function Binding
     (This : Inference_Context;
      Item : not null access constant Leander.Core.Typeable.Abstraction'Class)
      return Core.Types.Reference;

   function Type_Env
     (This : Inference_Context)
      return Core.Type_Env.Reference;

   procedure Bind
     (This  : in out Inference_Context'Class;
      Item  : not null access constant Leander.Core.Typeable.Abstraction'Class;
      To    : Leander.Core.Types.Reference);

   procedure Set_Result
     (This  : in out Inference_Context'Class;
      Ty    : Leander.Core.Types.Reference);

   procedure Error
     (This    : in out Inference_Context'Class;
      Message : String);

   procedure Add_Error_Context
     (This    : in out Inference_Context'Class;
      Context : String);

   function Get_Type
     (This       : Inference_Context;
      Typeable   : not null access constant Core.Typeable.Abstraction'Class)
      return Leander.Core.Types.Reference;

   function Get_Qualified_Type
     (This       : Inference_Context;
      Typeable   : not null access constant Core.Typeable.Abstraction'Class)
      return Leander.Core.Qualified_Types.Reference;

   procedure Update_Type
     (This  : Inference_Context;
      Root  : not null access
        Leander.Core.Qualified_Types.Has_Qualified_Type'Class);

private

   type Nullable_Type_Reference is
     access constant Leander.Core.Types.Instance'Class;

   package Type_Maps is
     new Leander.Core.Typeable.Maps
       (Nullable_Type_Reference);

   package Env_Stacks is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Core.Type_Env.Reference,
        Leander.Core.Type_Env."=");

   package Predicate_Lists is
     new Ada.Containers.Doubly_Linked_Lists
       (Leander.Core.Predicates.Instance,
        Leander.Core.Predicates."=");

   type Inference_Context is tagged
      record
         Success       : Boolean := True;
         Error_Message : Leander.Names.Leander_Name;
         Expr_Types    : Type_Maps.Map;
         Inferred_Type : Nullable_Type_Reference;
         Type_Env      : Leander.Core.Type_Env.Reference;
         Env_Stack     : Env_Stacks.List;
         Subst         : Leander.Core.Substitutions.Instance :=
                           Leander.Core.Substitutions.Empty;
         Predicates    : Predicate_Lists.List := [];
      end record;

   function OK (This : Inference_Context) return Boolean
   is (This.Success);

   function Error_Message (This : Inference_Context) return String
   is (Leander.Names.To_String (This.Error_Message));

   function Get_Type
     (This     : Inference_Context;
      Typeable : not null access constant Core.Typeable.Abstraction'Class)
      return Leander.Core.Types.Reference
   is (Types.Reference (This.Expr_Types.Element (Typeable)));

   function Binding
     (This : Inference_Context;
      Item : not null access constant Leander.Core.Typeable.Abstraction'Class)
      return Core.Types.Reference
   is (Leander.Core.Types.Reference
       (This.Expr_Types.Element (Item)));

   function Type_Env
     (This : Inference_Context)
      return Core.Type_Env.Reference
   is (This.Type_Env);

   function Initial_Context
     (Type_Env : Leander.Core.Type_Env.Reference)
      return Inference_Context
   is (Inference_Context'
         (Type_Env => Type_Env, others => <>));

   function Get_Qualified_Type
     (This       : Inference_Context;
      Typeable   : not null access constant Core.Typeable.Abstraction'Class)
      return Leander.Core.Qualified_Types.Reference
   is (Leander.Core.Qualified_Types.Qualified_Type
       (This.Current_Predicates, This.Get_Type (Typeable)));

end Leander.Core.Inference;
