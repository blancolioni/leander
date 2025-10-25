package body Leander.Names.Maps is

   --------------
   -- Get_Keys --
   --------------

   function Get_Keys (Container : Map) return Leander.Names.Name_Array is
      Count : constant Natural := Natural (Container.Name_Map.Length);
      Last  : Natural := 0;
   begin
      return Result : Leander.Names.Name_Array (1 .. Count) do
         for Position in Container.Name_Map.Iterate loop
            Last := Last + 1;
            Result (Last) := Name_Maps.Key (Position);
         end loop;
         pragma Assert (Last = Count);
      end return;
   end Get_Keys;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (Container : in out Map;
      Key       : Leander_Name;
      Element   : Element_Type)
   is
   begin
      Container.Name_Map.Insert (Key, Element);
   end Insert;

   -------------
   -- Iterate --
   -------------

   procedure Iterate
     (Container : Map;
      Process   : not null access
        procedure (Key : Leander_Name;
                   Element : Element_Type))
   is
   begin
      for Position in Container.Name_Map.Iterate loop
         Process (Name_Maps.Key (Position), Name_Maps.Element (Position));
      end loop;
   end Iterate;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (Container : in out Map;
      Key       : Leander_Name;
      Element   : Element_Type)
   is
   begin
      Container.Name_Map.Replace (Key, Element);
   end Replace;

   ---------------
   -- Singleton --
   ---------------

   function Singleton
     (Key     : Leander_Name;
      Element : Element_Type)
      return Map
   is
   begin
      return This : Map do
         This.Name_Map.Insert (Key, Element);
      end return;
   end Singleton;

end Leander.Names.Maps;
