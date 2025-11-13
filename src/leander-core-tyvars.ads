private with Ada.Containers.Vectors;
with Leander.Core.Kinds;
with Leander.Core.Substitutions;
with Leander.Names;
with Leander.Showable;

package Leander.Core.Tyvars is

   type Instance is new Kinds.Has_Kind
     and Showable.Abstraction
   with private;

   type Tyvar_Array is array (Positive range <>) of Instance;

   function Name
     (This : Instance)
      return Varid;

   function Tyvar
     (Id   : Varid;
      Kind : Leander.Core.Kinds.Kind)
      return Instance;

   function New_Tyvar
     (Kind : Leander.Core.Kinds.Kind := Leander.Core.Kinds.Star)
     return Instance;

   type Container_Abstraction is interface and Leander.Showable.Abstraction;

   function Contains
     (This  : Container_Abstraction;
      Tyvar : Instance'Class)
      return Boolean
      is abstract;

   function Get_Tyvars
     (This  : Container_Abstraction)
      return Tyvar_Array
      is abstract;

   function Apply
     (This  : not null access constant Container_Abstraction;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Container_Abstraction
      is abstract;

   function Generate
     (This : not null access constant Container_Abstraction'Class)
      return access constant Container_Abstraction'Class;

   function "/"
     (Container : Tyvar_Array;
      Tvs       : Tyvar_Array)
      return Tyvar_Array;

   function Intersection
     (X, Y : Tyvar_Array)
      return Tyvar_Array;

   function Union
     (X, Y : Tyvar_Array)
      return Tyvar_Array;

   function Nub
     (Tvs : Tyvar_Array)
      return Tyvar_Array;
   --  remove duplicates from tvs

   type Tyvar_Array_Builder is tagged private;

   procedure Include (This : in out Tyvar_Array_Builder;
                      Tv   : Instance'Class);

   procedure Include (This : in out Tyvar_Array_Builder;
                      Tvs  : Tyvar_Array);

   function To_Tyvar_Array
     (This : Tyvar_Array_Builder)
      return Tyvar_Array;

private

   type Instance is new Kinds.Has_Kind
     and Showable.Abstraction with
      record
         Id    : Varid;
         Kind  : Leander.Core.Kinds.Kind;
      end record;

   overriding function Get_Kind
     (This : Instance)
      return Leander.Core.Kinds.Kind
   is (This.Kind);

   overriding function Show
     (This : Instance)
      return String
   is (To_String (This.Id));

   function Name
     (This : Instance)
      return Varid
   is (This.Id);

   function To_Varid (S : String) return Varid
   is (Varid (Leander.Names.To_Leander_Name (S)));

   package Tyvar_Vectors is
     new Ada.Containers.Vectors (Positive, Instance);

   type Tyvar_Array_Builder is tagged
      record
         Vector : Tyvar_Vectors.Vector;
      end record;

end Leander.Core.Tyvars;
