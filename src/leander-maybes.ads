generic
   type Element_Type is private;
package Leander.Maybes is

   type Maybe (<>) is tagged private;

   function Is_Nothing (This : Maybe'Class) return Boolean;
   function Is_Just (This : Maybe'Class) return Boolean;
   function From_Just (This : Maybe'Class) return Element_Type
     with Pre => This.Is_Just;

   function Just (Item : Element_Type) return Maybe;
   function Nothing return Maybe;

private

   type Maybe (Has_Element : Boolean) is tagged
      record
         case Has_Element is
            when False =>
               null;
            when True =>
               Element : Element_Type;
         end case;
      end record;

   function Is_Just (This : Maybe'Class) return Boolean
   is (This.Has_Element);

   function Is_Nothing (This : Maybe'Class) return Boolean
   is (not This.Is_Just);

   function From_Just (This : Maybe'Class) return Element_Type
   is (This.Element);

   function Just (Item : Element_Type) return Maybe
   is (Maybe'(True, Item));

   function Nothing return Maybe
   is (Maybe'(Has_Element => False));

end Leander.Maybes;
