with Leander.Core.Types;
with Leander.Logging;

package body Leander.Core.Substitutions is

   function Names
     (This : Instance'Class)
        return Leander.Names.Name_Array;

   -------------
   -- Compose --
   -------------

   function Compose
     (Left  : Instance;
      Right : Instance)
      return Instance
   is
      This : Instance;
   begin
      if Left.List.Is_Empty then
         return Right;
      elsif Right.List.Is_Empty then
         return Left;
      end if;

      Leander.Logging.Log
        ("SUBST", "left=" & Left.Show);
      Leander.Logging.Log
        ("SUBST", "right=" & Right.Show);
      for Sr of Right.List loop
         This.List.Append (Subst_Record'
                             (Sr.Name,
                              Sr.Ref.Apply (Left)));
      end loop;
      for Sr of Left.List loop
         This.List.Append (Subst_Record'
                             (Sr.Name,
                              Sr.Ref.Apply (Right)));
      end loop;

      Leander.Logging.Log
        ("SUBST", "composed=" & This.Show);
      return This;
   end Compose;

   -------------
   -- Compose --
   -------------

   function Compose
     (Name  : Leander.Names.Leander_Name;
      Ty    : not null access constant Leander.Core.Types.Instance'Class;
      Right : Instance)
      return Instance
   is
      Left : Instance;
   begin
      Left.List.Append (Subst_Record'(Name, Nullable_Type_Reference (Ty)));
      return Left.Compose (Right);
   end Compose;

   -----------
   -- Empty --
   -----------

   function Empty return Instance is
   begin
      return (others => <>);
   end Empty;

   ------------
   -- Lookup --
   ------------

   function Lookup
     (This : Instance;
      Name : Leander.Names.Leander_Name)
      return Nullable_Type_Reference
   is
      use type Leander.Names.Leander_Name;
   begin
      for Element of This.List loop
         if Element.Name = Name then
            return Element.Ref;
         end if;
      end loop;
      return null;
   end Lookup;

   -----------
   -- Merge --
   -----------

   function Merge
     (Left, Right : Instance;
      Success     : out Boolean)
        return Instance
   is
      function Agree return Boolean;

      -----------
      -- Agree --
      -----------

      function Agree return Boolean is
         Names : constant Leander.Names.Name_Array :=
                   Leander.Names.Intersection (Left.Names, Right.Names);
      begin
         for N of Names loop
            declare
               T1 : constant Nullable_Type_Reference := Left.Lookup (N);
               T2 : constant Nullable_Type_Reference := Right.Lookup (N);
            begin
               if not T1.Equivalent (T2) then
                  return False;
               end if;
            end;
         end loop;
         return True;
      end Agree;

   begin

      if not Agree then
         Success := False;
         return Empty;
      end if;

      Success := True;
      return Right.Compose (Left);
   end Merge;

   -----------
   -- Names --
   -----------

   function Names
     (This : Instance'Class)
        return Leander.Names.Name_Array
   is
      Result : Leander.Names.Name_Array (1 .. Natural (This.List.Length)) :=
                 (if This.List.Is_Empty then []
                  else [for N of This.List => N.Name]);
      Last   : Natural := 0;
   begin
      for Subst of This.List loop
         declare
            use type Leander.Names.Leander_Name;
            Found : Boolean := False;
         begin
            for I in 1 .. Last loop
               if Result (I) = Subst.Name then
                  Found := True;
                  exit;
               end if;
            end loop;
            if not Found then
               Last := Last + 1;
               Result (Last) := Subst.Name;
            end if;
         end;
      end loop;
      return Result (1 .. Last);
   end Names;

   ----------
   -- Show --
   ----------

   overriding function Show (This : Instance) return String is
      function Show (Position : Subst_Lists.Cursor) return String;

      ----------
      -- Show --
      ----------

      function Show (Position : Subst_Lists.Cursor) return String is
         use Subst_Lists;
      begin
         if not Has_Element (Position) then
            return "";
         else
            declare
               E   : constant Subst_Record := Element (Position);
               Img : constant String :=
                       Leander.Names.To_String (E.Name)
                     & ":"
                       & E.Ref.Show;
            begin
               if Has_Element (Next (Position)) then
                  return Img & ";" & Show (Next (Position));
               else
                  return Img;
               end if;
            end;
         end if;
      end Show;

   begin
      return "{" & Show (This.List.First) & "}";
   end Show;

   -------------
   -- Without --
   -------------

   function Without
     (This : Instance;
      Tvs  : Leander.Names.Name_Array)
      return Instance
   is

      function Exclude (Name : Leander.Names.Leander_Name) return Boolean;

      -------------
      -- Exclude --
      -------------

      function Exclude (Name : Leander.Names.Leander_Name) return Boolean is
         use type Leander.Names.Leander_Name;
      begin
         for N of Tvs loop
            if N = Name then
               return True;
            end if;
         end loop;
         return False;
      end Exclude;

   begin
      return Result : Instance do
         for Subst of This.List loop
            if not Exclude (Subst.Name) then
               Result.List.Append (Subst);
            end if;
         end loop;
      end return;
   end Without;

end Leander.Core.Substitutions;
