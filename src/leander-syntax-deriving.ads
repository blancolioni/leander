with Leander.Data_Types;
with Leander.Parser;
with Leander.Source;

package Leander.Syntax.Deriving is

   procedure Derive_Eq
     (Context : Leander.Parser.Parse_Context'Class;
      Loc     : Leander.Source.Source_Location;
      DT      : Leander.Data_Types.Reference);

end Leander.Syntax.Deriving;
