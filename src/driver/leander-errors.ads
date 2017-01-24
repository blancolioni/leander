with Leander.Source;

package Leander.Errors is

   function Has_Errors return Boolean;
   procedure Clear_Errors;

   procedure Error
     (Source  : Leander.Source.Source_Reference;
      Message : String);

end Leander.Errors;
