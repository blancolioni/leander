package body Leander.Core.Expressions.Maps is

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Key       : Reference;
      Element   : Element_Type)
   is
   begin
      Container.Inner.Insert (Key.Id, Element);
   end Insert;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : Reference;
      Element   : Element_Type)
   is
   begin
      Container.Inner.Replace (Key.Id, Element);
   end Replace;

end Leander.Core.Expressions.Maps;
