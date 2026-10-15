with Leander.Data_Types;
with Leander.Syntax.Patterns.Constructors;
with Leander.Syntax.Records;

package body Leander.Syntax.Patterns.Records is

   use type Leander.Data_Types.Reference;

   ----------------------
   -- Constructor_Args --
   ----------------------

   overriding function Constructor_Args
     (This : Instance)
      return Reference_Array
   is
      Con_Name : constant String := Leander.Names.To_String (This.Name);
      DT       : constant Leander.Data_Types.Reference :=
                   Syntax.Records.Constructor_Type (This.Env, Con_Name);
   begin
      if DT = null then
         return [for Field of This.Fields => Field.Pattern];
      end if;

      declare
         Index  : constant Positive :=
                    Syntax.Records.Constructor_Index (DT, Con_Name);
         Args   : Reference_Array (1 .. DT.Constructor_Arity (Index)) :=
                    [others => null];
         Report : constant Boolean := not This.Reported.all;
      begin
         for Field of This.Fields loop
            declare
               Label    : constant String :=
                            Syntax.Records.Bare_Name
                              (Leander.Names.To_String (Field.Label));
               Position : constant Natural :=
                            DT.Label_Position (Index, Label);
            begin
               if Position = 0 then
                  if Report then
                     Syntax.Records.Report
                       (This.Location,
                        "constructor " & Con_Name & " has no field "
                        & Label);
                  end if;
               elsif Args (Position) /= null then
                  if Report then
                     Syntax.Records.Report
                       (This.Location,
                        "field " & Label & " is given more than once");
                  end if;
               else
                  Args (Position) := Field.Pattern;
               end if;
            end;
         end loop;
         This.Reported.all := True;

         for Arg of Args loop
            if Arg = null then
               Arg := Wildcard (This.Location);
            end if;
         end loop;
         return Args;
      end;
   end Constructor_Args;

   --------------------
   -- Record_Pattern --
   --------------------

   function Record_Pattern
     (Location    : Leander.Source.Source_Location;
      Constructor : String;
      Fields      : Field_Pattern_Array;
      Env         : Leander.Environment.Reference)
      return Reference
   is (Reference
         (Syntax.Allocate
            (Instance'
               (Count    => Fields'Length,
                Location => Location,
                Name     => Leander.Names.To_Leander_Name (Constructor),
                Fields   => Fields,
                Env      => Env,
                Reported => new Boolean'(False)))));

   -------------
   -- To_Core --
   -------------

   overriding function To_Core
     (This : Instance)
      return Leander.Core.Patterns.Reference
   is (Constructors.Constructor
         (This.Location, Leander.Names.To_String (This.Name),
          This.Constructor_Args).To_Core);

end Leander.Syntax.Patterns.Records;
