with Ada.Strings.Fixed;

package body Leander.Scopes is

   ----------------
   -- Add_Module --
   ----------------

   procedure Add_Module
     (This  : in out Instance;
      Alias : String;
      Env   : not null Leander.Environment.Reference)
   is
   begin
      if not This.Modules.Contains (Alias) then
         This.Modules.Insert (Alias, Env);
      end if;
   end Add_Module;

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

   ---------------
   -- New_Scope --
   ---------------

   function New_Scope return Reference is
   begin
      return new Instance;
   end New_Scope;

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
      pragma Unreferenced (Space);
   begin
      Message := Null_Unbounded_String;

      if not Is_Qualified (Written) then
         --  An unqualified name is left exactly as written. It may be a
         --  top-level name this module declares, a lambda or where
         --  binding, or the left-hand side of a declaration -- and at the
         --  point of use they are indistinguishable. Which imported names
         --  can be reached bare is settled by Environment.Import instead,
         --  by what it does and does not insert under a bare key.
         Name := To_Unbounded_String (Written);
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
                  declare
                     Env : constant Leander.Environment.Reference :=
                             This.Modules (Qualifier);
                  begin
                     --  Is_Exported, not Declares: a name the module
                     --  keeps to itself is no more reachable behind a
                     --  qualifier than it is bare.
                     if Env.Is_Exported (Simple) then
                        Name :=
                          To_Unbounded_String (Env.Canonical_Name (Simple));
                     else
                        Name := To_Unbounded_String (Written);
                        Message :=
                          To_Unbounded_String
                            ("module " & Qualifier & " does not export "
                             & Simple);
                     end if;
                  end;
                  return;
               end if;
            end;
         end if;
      end loop;

      Name := To_Unbounded_String (Written);
      Message :=
        To_Unbounded_String
          ("no imported module is named "
           & Written (Written'First
                      .. Ada.Strings.Fixed.Index
                           (Written, ".", Ada.Strings.Backward) - 1));
   end Resolve;

end Leander.Scopes;
