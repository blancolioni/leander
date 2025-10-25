package body Leander.Core.Typeable.Maps is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Key       : not null access constant Abstraction'Class;
      Element   : Element_Type)
   is
   begin
      Container.Inner.Insert (Key.Get_Id, Element);
   end Insert;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : not null access constant Abstraction'Class;
      Element   : Element_Type)
   is
   begin
      Container.Inner.Replace (Key.Get_Id, Element);
   end Replace;

end Leander.Core.Typeable.Maps;
