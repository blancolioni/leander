with Ada.Strings.Fixed;

with Leander.Names;

package body Leander.Scopes is

   function Holds
     (Env   : not null Leander.Environment.Reference;
      Name  : String;
      Space : Name_Space)
      return Boolean;
   --  Whether Env has Name in the given namespace. Values need
   --  Variable_Binding_Exists: Environment.Exists answers False for
   --  Variable_Binding unconditionally.

   ----------------
   -- Add_Module --
   ----------------

   procedure Add_Module
     (This  : in out Instance;
      Alias : String;
      Env   : not null Leander.Environment.Reference)
   is
   begin
      --  Importing the same module twice, or two modules under one alias,
      --  leaves the first: a later import cannot take a qualifier away
      --  from an earlier one.
      if not This.Modules.Contains (Alias) then
         This.Modules.Insert (Alias, Env);
      end if;
   end Add_Module;

   -----------
   -- Holds --
   -----------

   function Holds
     (Env   : not null Leander.Environment.Reference;
      Name  : String;
      Space : Name_Space)
      return Boolean
   is
      L : constant Leander.Names.Leander_Name :=
            Leander.Names.To_Leander_Name (Name);
   begin
      case Space is
         when Value_Space =>
            return Env.Variable_Binding_Exists (Name);
         when Constructor_Space =>
            return Env.Exists (L, Leander.Environment.Constructor);
         when Type_Space =>
            return Env.Exists (L, Leander.Environment.Type_Constructor);
         when Class_Space =>
            return Env.Exists (L, Leander.Environment.Class_Binding);
      end case;
   end Holds;

   ------------------
   -- Is_Qualified --
   ------------------

   function Is_Qualified (Written : String) return Boolean is
      Dot : constant Natural :=
              Ada.Strings.Fixed.Index (Written, ".");
   begin
      --  Only a leading component that looks like a module name makes this
      --  a qualified name; Scan_Qualified_Name has already applied the
      --  same rule when deciding to join the run at all.
      return Dot > Written'First
        and then Written (Written'First) in 'A' .. 'Z';
   end Is_Qualified;

   -------------
   -- Resolve --
   -------------

   procedure Resolve
     (This    : Instance;
      Written : String;
      Space   : Name_Space;
      Name    : out Ada.Strings.Unbounded.Unbounded_String;
      Message : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Name := To_Unbounded_String (Written);
      Message := Null_Unbounded_String;

      if not Is_Qualified (Written) then
         return;
      end if;

      --  Longest qualifier first, so that "Data.Map.Map" prefers module
      --  "Data.Map" over module "Data".
      for Split in reverse Written'First + 1 .. Written'Last - 1 loop
         if Written (Split) = '.' then
            declare
               Qualifier : constant String :=
                             Written (Written'First .. Split - 1);
               Simple    : constant String :=
                             Written (Split + 1 .. Written'Last);
            begin
               if This.Modules.Contains (Qualifier) then
                  if Holds (This.Modules (Qualifier), Simple, Space) then
                     Name := To_Unbounded_String (Simple);
                  else
                     Message :=
                       To_Unbounded_String
                         ("module " & Qualifier & " does not export "
                          & Simple);
                  end if;
                  return;
               end if;
            end;
         end if;
      end loop;

      Message :=
        To_Unbounded_String
          ("no imported module is named "
           & Written (Written'First
                      .. Ada.Strings.Fixed.Index
                           (Written, ".", Ada.Strings.Backward) - 1));
   end Resolve;

   ---------------
   -- New_Scope --
   ---------------

   function New_Scope return Reference is
   begin
      return new Instance;
   end New_Scope;

end Leander.Scopes;
