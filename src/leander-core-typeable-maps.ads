private with Ada.Containers.Ordered_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Leander.Core.Typeable.Maps is

   type Map is tagged private;

   function Contains
     (Container : Map;
      Key       : not null access constant Abstraction'Class)
      return Boolean;

   function Element
     (Container : Map;
      Key       : not null access constant Abstraction'Class)
      return Element_Type
     with Pre => Container.Contains (Key);

   procedure Insert
     (Container : in out Map;
      Key       : not null access constant Abstraction'Class;
      Element   : Element_Type)
     with Pre => not Container.Contains (Key),
       Post => Container.Contains (Key);

   procedure Replace
     (Container : in out Map;
      Key       : not null access constant Abstraction'Class;
      Element   : Element_Type)
     with Pre => Container.Contains (Key),
       Post => Container.Contains (Key);

private

   package Expr_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Typeable_Id,
        Element_Type => Element_Type,
        "<"          => "<",
        "="          => "=");

   type Map is tagged
      record
         Inner : Expr_Maps.Map;
      end record;

   function Contains
     (Container : Map;
      Key       : not null access constant Abstraction'Class)
      return Boolean
   is (Container.Inner.Contains (Key.Get_Id));

   function Element
     (Container : Map;
      Key       : not null access constant Abstraction'Class)
      return Element_Type
   is (Container.Inner.Element (Key.Get_Id));

end Leander.Core.Typeable.Maps;
