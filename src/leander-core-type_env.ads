with Leander.Core.Schemes;
with Leander.Core.Substitutions;
with Leander.Core.Tyvars;
with Leander.Disposable;
with Leander.Names;
with Leander.Names.Maps;
with Leander.Showable;

package Leander.Core.Type_Env is

   type Instance is
     new Leander.Showable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Core.Tyvars.Container_Abstraction
   with private;

   type Reference is access constant Instance;

   type Nullable_Scheme_Reference is
     access constant Leander.Core.Schemes.Instance'Class;

   function Empty return Reference;

   function Lookup
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Nullable_Scheme_Reference;

   function Contains
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Boolean;

   function Element
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Leander.Core.Schemes.Reference
     with Pre => This.Contains (Name);

   function Compose
     (This   : not null access constant Instance'Class;
      Name   : Varid;
      Scheme : Leander.Core.Schemes.Reference)
      return access constant Instance;

   function Compose
     (This   : not null access constant Instance'Class;
      Name   : String;
      Scheme : Leander.Core.Schemes.Reference)
      return access constant Instance;

   function Compose
     (This   : not null access constant Instance'Class;
      That   : not null access constant Instance'Class)
      return access constant Instance;

   function Save
     (This    : not null access constant Instance'Class;
      To      : not null access constant Instance'Class;
      Subst   : Leander.Core.Substitutions.Instance'Class)
      return Reference;

   function Ids (This : Instance'Class) return Leander.Names.Name_Array;

   type Builder is tagged private;

   function Get_Type_Env
     (This : Builder)
      return Reference;

   procedure Prune;

private

   package Scheme_Maps is
     new Leander.Names.Maps (Nullable_Scheme_Reference);

   type Instance is
     new Leander.Showable.Abstraction
     and Leander.Disposable.Abstraction
     and Leander.Core.Tyvars.Container_Abstraction with
      record
         Map  : Scheme_Maps.Map;
         Next : Reference;
      end record;

   overriding function Show (This : Instance) return String;

   overriding function Contains
     (This  : Instance;
      Tyvar : Leander.Core.Tyvars.Instance'Class)
      return Boolean;

   overriding function Get_Tyvars
     (This  : Instance)
      return Leander.Core.Tyvars.Tyvar_Array;

   overriding function Apply
     (This  : not null access constant Instance;
      Subst : Leander.Core.Substitutions.Instance'Class)
      return access constant Instance;

   overriding procedure Dispose (This : in out Instance);

   type Builder is tagged
      record
         Type_Env : Instance;
      end record;

   function Compose
     (This   : not null access constant Instance'Class;
      Name   : String;
      Scheme : Leander.Core.Schemes.Reference)
      return access constant Instance
   is (This.Compose (To_Varid (Name), Scheme));

end Leander.Core.Type_Env;
