private generic
   Element_Name : String;
   type Element_Type (<>) is private;
   with function At_Element return Boolean;
   with function Parse return Element_Type;
package Leander.Parser.Sequences is

   procedure Parse_Sequence
     (On_Parsed : not null access
        procedure (Element : Element_Type));

end Leander.Parser.Sequences;
