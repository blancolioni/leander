package Leander.IO is

   type Abstraction is interface;
   type Reference is access all Abstraction'Class;

   procedure Put
     (This       : Abstraction;
      File_Index : Natural;
      Value      : Wide_Wide_Character)
   is abstract;

   function Null_IO return Reference;
   function Local_IO return Reference;

end Leander.IO;
