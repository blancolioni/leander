limited with Leander.Core.Binding_Groups;
with Leander.Core.Literals;
with Leander.Core.Typeable;
with Leander.Disposable;
with Leander.Showable;
with Leander.Traverseable;

package Leander.Core.Patterns is

   type Instance (<>) is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Traverseable.Abstraction
   with private;

   type Reference is not null access constant Instance'Class;
   type Reference_Array is array (Positive range <>) of Reference;

   type Conargs is array (Positive range <>) of Varid;

   function Is_Variable (This : Instance) return Boolean;
   function Is_Wildcard (This : Instance) return Boolean;
   function Variable (This : Instance) return Varid
     with Pre => This.Is_Variable;

   function Is_Constructor (This : Instance) return Boolean;
   function Constructor (This : Instance) return Conid
     with Pre => This.Is_Constructor;
   function Con_Arguments (This : Instance) return Conargs
     with Pre => This.Is_Constructor;

   function Is_Literal (This : Instance) return Boolean;
   function Literal (This : Instance) return Literals.Instance
     with Pre => This.Is_Literal;

   function Variable (Id : Varid) return Reference;

   function Constructor
     (Id   : Conid;
      Args : Conargs)
      return Reference;

   function Literal (Lit : Literals.Instance) return Reference;

   function Has_Reference
     (This : Instance'Class;
      To   : Varid)
      return Boolean;

   procedure Prune;

private

   type Instance_Tag is (PLit, PVar, PCon);

   type Binding_Group_Reference is
     access constant Leander.Core.Binding_Groups.Instance'Class;

   type Instance (Tag : Instance_Tag; Arg_Count : Natural) is
     new Leander.Showable.Abstraction
     and Leander.Core.Typeable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Traverseable.Abstraction with
      record
         Id : Core.Typeable.Typeable_Id;
         case Tag is
            when PVar =>
               Var_Id      : Varid;
            when PCon =>
               Con_Id      : Conid;
               Con_Args    : Conargs (1 .. Arg_Count);
            when PLit =>
               Literal     : Leander.Core.Literals.Instance;
         end case;
      end record;

   overriding function Show
     (This : Instance)
      return String;

   overriding procedure Dispose (This : in out Instance);

   overriding function Get_Id
     (This : Instance)
      return Leander.Core.Typeable.Typeable_Id
   is (This.Id);

   overriding procedure Traverse
     (This    : not null access constant Instance;
      Process : not null access
        procedure (This : not null access constant
                     Traverseable.Abstraction'Class));

   function Allocate
     (This : Instance)
      return Reference;

   function Is_Variable (This : Instance) return Boolean
   is (This.Tag = PVar);

   function Is_Wildcard (This : Instance) return Boolean
   is (This.Tag = PVar
       and then To_String (This.Var_Id) = "_");

   function Variable (This : Instance) return Varid
   is (This.Var_Id);

   function Is_Literal (This : Instance) return Boolean
   is (This.Tag = PLit);

   function Literal (This : Instance) return Literals.Instance
   is (This.Literal);

   function Is_Constructor (This : Instance) return Boolean
   is (This.Tag = PCon);

   function Constructor (This : Instance) return Conid
   is (This.Con_Id);

   function Con_Arguments (This : Instance) return Conargs
   is (This.Con_Args);

end Leander.Core.Patterns;
