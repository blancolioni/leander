with Ada.Wide_Wide_Text_IO;
package body Leander.IO is

   type Null_IO_Instance is new Abstraction with null record;

   overriding procedure Put
     (This       : Null_IO_Instance;
      File_Index : Natural;
      Value      : Wide_Wide_Character)
   is null;

   Singleton_Null_IO : aliased Null_IO_Instance := (null record);

   type Local_IO_Instance is new Abstraction with null record;

   overriding procedure Put
     (This       : Local_IO_Instance;
      File_Index : Natural;
      Value      : Wide_Wide_Character);

   Singleton_Local_IO : aliased Local_IO_Instance := (null record);

   function Null_IO return Reference
   is (Singleton_Null_IO'Access);

   function Local_IO return Reference
   is (Singleton_Local_IO'Access);

   ---------
   -- Put --
   ---------

   overriding procedure Put
     (This       : Local_IO_Instance;
      File_Index : Natural;
      Value      : Wide_Wide_Character)
   is
   begin
      Ada.Wide_Wide_Text_IO.Put (Value);
   end Put;

end Leander.IO;
