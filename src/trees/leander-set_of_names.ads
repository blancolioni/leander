with Ada.Strings.Fixed.Hash;
with Ada.Containers.Indefinite_Hashed_Sets;

package Leander.Set_Of_Names is
  new Ada.Containers.Indefinite_Hashed_Sets
    (Element_Type        => String,
     Hash                => Ada.Strings.Fixed.Hash,
     Equivalent_Elements => "=");
