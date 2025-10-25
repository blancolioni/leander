package body Leander.Core.Typeable is

   Next_Id : Typeable_Id := Typeable_Id'First;

   ------------
   -- New_Id --
   ------------

   function New_Id return Typeable_Id is
   begin
      return Id : constant Typeable_Id := Next_Id do
         Next_Id := @ + 1;
      end return;
   end New_Id;

end Leander.Core.Typeable;
