with Ada.Strings.Unbounded;

with Leander.Data_Types;
with Leander.Syntax.Records;

package body Leander.Syntax.Expressions.Records is

   use Ada.Strings.Unbounded;
   use type Leander.Data_Types.Reference;
   use type Leander.Environment.Reference;

   function Fresh return String
   is (Leander.Names.To_String (Leander.Names.New_Name));

   function Call_Error
     (Location : Leander.Source.Source_Location;
      Message  : String)
      return Reference
   is (Application
         (Location, Variable (Location, "error"),
          String_Literal (Location, Message)));
   --  error "Message", for a field that is missing or cannot be updated.

   function Label_Image (Label : Leander.Names.Leander_Name) return String
   is (Syntax.Records.Bare_Name (Leander.Names.To_String (Label)));
   --  A label as its type declares it, without any module qualifier.

   ------------------
   -- Construction --
   ------------------

   function Construction
     (Location    : Leander.Source.Source_Location;
      Constructor : String;
      Fields      : Field_Expression_Array;
      Env         : Leander.Environment.Reference)
      return Reference
   is (Reference
         (Syntax.Allocate
            (Construction_Instance'
               (Count       => Fields'Length,
                Location    => Location,
                Constructor => Leander.Names.To_Leander_Name (Constructor),
                Fields      => Fields,
                Env         => Env))));

   ------------
   -- Update --
   ------------

   function Update
     (Location : Leander.Source.Source_Location;
      Value    : Reference;
      Fields   : Field_Expression_Array;
      Env      : Leander.Environment.Reference)
      return Reference
   is (Reference
         (Syntax.Allocate
            (Update_Instance'
               (Count    => Fields'Length,
                Location => Location,
                Value    => Value,
                Fields   => Fields,
                Env      => Env))));

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Construction_Instance)
      return Leander.Core.Expressions.Reference
   is
      Con_Name : constant String :=
                   Leander.Names.To_String (This.Constructor);
      DT       : constant Leander.Data_Types.Reference :=
                   Syntax.Records.Constructor_Type (This.Env, Con_Name);
      Result   : Reference := Constructor (This.Location, Con_Name);
   begin
      if DT = null then
         --  Not a constructor in scope: inference says so.
         return Result.To_Core;
      end if;

      declare
         Index  : constant Positive :=
                    Syntax.Records.Constructor_Index (DT, Con_Name);
         Labels : constant Leander.Names.Name_Array :=
                    DT.Constructor_Labels (Index);
         Args   : Reference_Array (1 .. DT.Constructor_Arity (Index)) :=
                    [others => null];
      begin
         for Field of This.Fields loop
            declare
               Label    : constant String := Label_Image (Field.Label);
               Position : constant Natural :=
                            DT.Label_Position (Index, Label);
            begin
               if Position = 0 then
                  Syntax.Records.Report
                    (This.Location,
                     "constructor " & Con_Name & " has no field "
                     & Label);
               elsif Args (Position) /= null then
                  Syntax.Records.Report
                    (This.Location,
                     "field " & Label & " is given more than once");
               else
                  Args (Position) := Field.Value;
               end if;
            end;
         end loop;

         --  A field left out is bottom: an error if it is ever used.
         for I in Args'Range loop
            Result :=
              Application
                (This.Location, Result,
                 (if Args (I) /= null then Args (I)
                  else Call_Error
                    (This.Location,
                     "Missing field in record construction"
                     & (if Labels'Length > 0
                        then " " & Label_Image (Labels (I))
                        else ""))));
         end loop;
         return Result.To_Core;
      end;
   end To_Core;

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Update_Instance)
      return Leander.Core.Expressions.Reference
   is
      First : constant String :=
                (if This.Count = 0 then ""
                 else Label_Image (This.Fields (1).Label));
      DT    : constant Leander.Data_Types.Reference :=
                (if This.Env = null or else First = "" then null
                 else This.Env.Label_Data_Type (First));
      Type_Name : constant String :=
                    (if DT = null then ""
                     else Leander.Core.To_String (DT.Id));
      --  Written qualified, the labels come from a module that may only
      --  be in scope qualified, and its constructors are named so too.
      Qualifier : constant String :=
                    (if This.Count = 0 then ""
                     else Syntax.Records.Qualifier
                       (Leander.Names.To_String (This.Fields (1).Label)));
      Valid : Boolean := True;
   begin
      if This.Count = 0 then
         Syntax.Records.Report
           (This.Location, "a record update needs at least one field");
         return This.Value.To_Core;
      elsif DT = null then
         Syntax.Records.Report
           (This.Location, First & " is not a field of any type in scope");
         return This.Value.To_Core;
      end if;

      for I in This.Fields'Range loop
         declare
            Label : constant String := Label_Image (This.Fields (I).Label);
         begin
            if not DT.Has_Label (Label) then
               Syntax.Records.Report
                 (This.Location,
                  Label & " is not a field of " & Type_Name);
               Valid := False;
            end if;
            for J in This.Fields'First .. I - 1 loop
               if Label_Image (This.Fields (J).Label) = Label then
                  Syntax.Records.Report
                    (This.Location,
                     "field " & Label & " is given more than once");
                  Valid := False;
               end if;
            end loop;
         end;
      end loop;

      if not Valid then
         return This.Value.To_Core;
      end if;

      --  let f (C x1 .. xn) = C x1 .. e .. xn, one equation for each
      --  constructor with every field named, in f value.  Monomorphic, as
      --  the binding is made for this one use.
      declare
         Fn       : constant String := Fresh;
         Group    : constant Leander.Syntax.Bindings.Reference :=
                      Leander.Syntax.Bindings.Empty (Monomorphic => True);
         Eligible : Natural := 0;
         Names    : Unbounded_String;
      begin
         for I in This.Fields'Range loop
            Names := Names & (if I = 1 then "" else ", ")
              & Label_Image (This.Fields (I).Label);
         end loop;

         for Index in 1 .. DT.Constructor_Count loop
            if (for all Field of This.Fields =>
                  DT.Label_Position (Index, Label_Image (Field.Label)) > 0)
            then
               Eligible := Eligible + 1;
               declare
                  Con_Name : constant String :=
                               (if Qualifier = "" then ""
                                else Qualifier & ".")
                               & Leander.Core.To_String
                                 (DT.Constructor_Name (Index));
                  Arity    : constant Natural :=
                               DT.Constructor_Arity (Index);
                  Vars     : array (1 .. Arity) of Unbounded_String;
                  Pats     : Leander.Syntax.Patterns.Reference_Array
                               (1 .. Arity);
                  Rebuilt  : Reference := Constructor (This.Location, Con_Name);
               begin
                  for K in 1 .. Arity loop
                     Vars (K) := To_Unbounded_String (Fresh);
                     Pats (K) := Leander.Syntax.Patterns.Variable
                       (This.Location, To_String (Vars (K)));
                  end loop;

                  for K in 1 .. Arity loop
                     declare
                        Arg : Reference :=
                                Variable (This.Location, To_String (Vars (K)));
                     begin
                        for Field of This.Fields loop
                           if DT.Label_Position
                             (Index, Label_Image (Field.Label)) = K
                           then
                              Arg := Field.Value;
                           end if;
                        end loop;
                        Rebuilt := Application (This.Location, Rebuilt, Arg);
                     end;
                  end loop;

                  Group.Add_Binding
                    (This.Location, Fn,
                     [Leander.Syntax.Patterns.Constructor
                        (This.Location, Con_Name, Pats)],
                     Rebuilt);
               end;
            end if;
         end loop;

         if Eligible = 0 then
            Syntax.Records.Report
              (This.Location,
               "no constructor of " & Type_Name & " has all of the fields "
               & To_String (Names));
            return This.Value.To_Core;
         elsif Eligible < DT.Constructor_Count then
            Group.Add_Binding
              (This.Location, Fn,
               [Leander.Syntax.Patterns.Wildcard (This.Location)],
               Call_Error
                 (This.Location,
                  "No match in record update of " & To_String (Names)));
         end if;

         return Let
           (This.Location, Group,
            Application
              (This.Location, Variable (This.Location, Fn), This.Value))
             .To_Core;
      end;
   end To_Core;

   ----------------
   -- To_Pattern --
   ----------------

   overriding function To_Pattern
     (This : Construction_Instance)
      return Leander.Syntax.Patterns.Reference
   is
      Fields : Leander.Syntax.Patterns.Field_Pattern_Array (This.Fields'Range);
   begin
      for I in Fields'Range loop
         Fields (I) := (Label   => This.Fields (I).Label,
                        Pattern => This.Fields (I).Value.To_Pattern);
      end loop;
      return Leander.Syntax.Patterns.Record_Pattern
        (This.Location,
         Leander.Names.To_String (This.Constructor),
         Fields,
         This.Env);
   end To_Pattern;

end Leander.Syntax.Expressions.Records;
