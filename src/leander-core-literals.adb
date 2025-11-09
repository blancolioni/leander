package body Leander.Core.Literals is

   function Make (Tag : Instance_Tag;
                  Image : String)
                  return Instance
   is (Tag, Ada.Strings.Unbounded.To_Unbounded_String (Image));

   -----------------------
   -- Character_Literal --
   -----------------------

   function Character_Literal
     (Code : Natural)
      return Instance
   is
   begin
      return Make (LChar, Code'Image);
   end Character_Literal;

   -------------------
   -- Float_Literal --
   -------------------

   function Float_Literal
     (Image : String)
      return Instance
   is
   begin
      return Make (LFloat, Image);
   end Float_Literal;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (This : Instance)
      return Leander.Core.Types.Reference
   is
   begin
      case This.Tag is
         when LChar =>
            return Types.T_Char;
         when LFloat =>
            return Types.T_Double;
         when LInteger =>
            return Types.T_Int;
         when LString =>
            return Types.T_String;
      end case;
   end Get_Type;

   ---------------------
   -- Integer_Literal --
   ---------------------

   function Integer_Literal
     (Image : String)
      return Instance
   is
   begin
      return Make (LInteger, Image);
   end Integer_Literal;

   --------------------
   -- String_Literal --
   --------------------

   function String_Literal
     (S : String)
      return Instance
   is
   begin
      return Make (LString, S);
   end String_Literal;

   -----------------
   -- To_Calculus --
   -----------------

   function To_Calculus
     (This : Instance)
      return Leander.Calculus.Tree
   is
      use Leander.Calculus;
      Image : constant String :=
                Ada.Strings.Unbounded.To_String (This.Image);
   begin
      case This.Tag is
         when LChar =>
            return Number (Integer'Value (Image));
         when LFloat =>
            return Number (Integer (Float'Value (Image)));
         when LInteger =>
            return Number (Integer'Value (Image));
         when LString =>
            declare
               T : Tree := Symbol ("[]");
            begin
               for Ch of reverse Image loop
                  T :=
                    Apply
                      (Apply
                         (Symbol (":"),
                          Number (Character'Pos (Ch))),
                       T);
               end loop;
               return T;
            end;
      end case;
   end To_Calculus;

end Leander.Core.Literals;
