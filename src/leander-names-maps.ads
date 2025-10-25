private with Ada.Containers.Ordered_Maps;

generic
   type Element_Type is private;
   with function "=" (Left, Right : Element_Type) return Boolean is <>;
package Leander.Names.Maps is

   type Map is tagged private;

   function Singleton
     (Key     : Leander_Name;
      Element : Element_Type)
      return Map;

   function Contains
     (Container : Map;
      Key       : Leander_Name)
      return Boolean;

   function Element
     (Container : Map;
      Key       : Leander_Name)
      return Element_Type
     with Pre => Container.Contains (Key);

   procedure Insert
     (Container : in out Map;
      Key       : Leander_Name;
      Element   : Element_Type)
     with Pre => not Container.Contains (Key);

   procedure Replace
     (Container : in out Map;
      Key       : Leander_Name;
      Element   : Element_Type)
     with Pre => Container.Contains (Key),
     Post => Container.Contains (Key)
     and then Container.Element (Key) = Element;

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure (Key : Leander_Name;
                   Element : Element_Type));

   function Get_Keys (Container : Map) return Leander.Names.Name_Array;

private

   package Name_Maps is
     new Ada.Containers.Ordered_Maps
       (Key_Type     => Leander_Name,
        Element_Type => Element_Type,
        "<"          => "<",
        "="          => "=");

   type Map is tagged
      record
         Name_Map : Name_Maps.Map;
      end record;

   function Contains
     (Container : Map;
      Key       : Leander_Name)
      return Boolean
   is (Container.Name_Map.Contains (Key));

   function Element
     (Container : Map;
      Key       : Leander_Name)
      return Element_Type
   is (Container.Name_Map.Element (Key));

end Leander.Names.Maps;
