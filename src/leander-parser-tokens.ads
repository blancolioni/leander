private package Leander.Parser.Tokens is

   type Token is
      (Tok_None, Tok_End_Of_File, Tok_Bad_Character,
       Tok_Identifier,
       Tok_String_Literal,
       Tok_Character_Literal,
       Tok_Integer_Literal,
       Tok_Float_Literal,
       Tok_Else, Tok_If, Tok_Then, Tok_Case, Tok_Of, Tok_Do, Tok_Type,
       Tok_Class, Tok_Where, Tok_Instance, Tok_Data, Tok_Deriving,
       Tok_Import, Tok_In, Tok_Infix, Tok_Infixl, Tok_Infixr,
       Tok_Let, Tok_Module, Tok_Newtype, Tok_Foreign,
       Tok_Equal, Tok_Lambda, Tok_Dot_Dot, Tok_Vertical_Bar,
       Tok_Right_Arrow, Tok_Left_Arrow,
       Tok_Double_Right_Arrow, Tok_Colon_Colon,
       Tok_Left_Paren, Tok_Right_Paren, Tok_Comma, Tok_Semi,
       Tok_Left_Bracket, Tok_Right_Bracket,
       Tok_Back_Tick,
       Tok_Left_Brace, Tok_Right_Brace);

end Leander.Parser.Tokens;
