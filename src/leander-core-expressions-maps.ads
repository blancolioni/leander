private with Ada.Containers.Ordered_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Leander.Core.Expressions.Maps is

   type Map is tagged private;

   function Contains
     (Container : Map;
      Key       : Reference)
      return Boolean;

   function Element
     (Container : Map;
      Key       : Reference)
      return Element_Type
     with Pre => Container.Contains (Key);

   procedure Insert
     (Container : in out Map;
      Key       : Reference;
      Element   : Element_Type)
     with Pre => not Container.Contains (Key),
       Post => Container.Contains (Key);

   procedure Replace
     (Container : in out Map;
      Key       : Reference;
      Element   : Element_Type)
     with Pre => Container.Contains (Key),
       Post => Container.Contains (Key);

private

   package Expr_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Expr_Id,
        Element_Type => Element_Type,
        "<"          => "<",
        "="          => "=");

   type Map is tagged
      record
         Inner : Expr_Maps.Map;
      end record;

   function Contains
     (Container : Map;
      Key       : Reference)
      return Boolean
   is (Container.Inner.Contains (Key.Id));

   function Element
     (Container : Map;
      Key       : Reference)
      return Element_Type
   is (Container.Inner.Element (Key.Id));

end Leander.Core.Expressions.Maps;
