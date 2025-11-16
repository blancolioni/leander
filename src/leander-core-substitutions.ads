private with Ada.Containers.Doubly_Linked_Lists;

limited with Leander.Core.Types;
with Leander.Names;
with Leander.Showable;

package Leander.Core.Substitutions is

   type Instance is new Leander.Showable.Abstraction with private;

   function Empty return Instance;

   function Compose
     (Left  : Instance;
      Right : Instance)
      return Instance;

   function Compose
     (Name  : Leander.Names.Leander_Name;
      Ty    : not null access constant Leander.Core.Types.Instance'Class;
      Right : Instance)
      return Instance;

   function Singleton
     (Name  : Leander.Names.Leander_Name;
      Ty    : not null access constant Leander.Core.Types.Instance'Class)
      return Instance;

   function Without
     (This : Instance;
      Tvs  : Leander.Names.Name_Array)
      return Instance;

   function Merge
     (Left, Right : Instance;
      Success     : out Boolean)
        return Instance;

   type Nullable_Type_Reference is
     access constant Leander.Core.Types.Instance'Class;

   function Lookup
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Nullable_Type_Reference;

private

   type Subst_Record is
      record
         Name : Leander.Names.Leander_Name;
         Ref  : Nullable_Type_Reference;
      end record;

   package Subst_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Subst_Record);

   type Instance is new Leander.Showable.Abstraction with
      record
         List : Subst_Lists.List;
      end record;

   overriding function Show (This : Instance) return String;

   function Singleton
     (Name  : Leander.Names.Leander_Name;
      Ty    : not null access constant Leander.Core.Types.Instance'Class)
      return Instance
   is (Compose (Name, Ty, Empty));

end Leander.Core.Substitutions;
