with GCS.Styles;                       use GCS.Styles;
with GCS.Lexer;
pragma Elaborate_All (GCS.Lexer);

with Leander.Parser.Tokens;            use Leander.Parser.Tokens;

private package Leander.Parser.Lexical is
  new GCS.Lexer (Token              => Token,
                 Tok_None           => Tok_None,
                 Tok_End_Of_File    => Tok_End_Of_File,
                 Tok_Bad_Character  => Tok_Bad_Character,
                 Tok_Identifier     => Tok_Identifier,
                 Tok_String         => Tok_String_Literal,
                 Tok_Character      => Tok_Character_Literal,
                 Tok_Integer        => Tok_Integer_Literal,
                 Tok_Float          => Tok_Float_Literal,
                 First_Keyword      => Tok_Else,
                 Keywords           => "else if then case of do type " &
                                       "class where instance data deriving " &
                                       "import in infix infixl infixr " &
                                       "let module newtype foreign " &
                                       "= \ .. | -> <- => ::",
                 First_Symbol       => Tok_Left_Paren,
                 Symbols            => "( ) , ; [ ] ` { }",
                 Identifier_Start   => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "_:!#$%&*+./<=>?@\^|-~",
                 Identifier_Body    => "abcdefghijklmnopqrstuvwxyz" &
                                       "ABCDEFGHIJKLMNOPQRSTUVWXYZ" &
                                       "0123456789'" &
                                       "_:!#$%&*+./<=>?@\^|-~",
                 Identifier_Group   => ":!$%&*+./<=>?@\^|-~",
                 Line_Comment_Start => "--",
                 Escape_Character   => '\',
                 Properties         => [Case_Sensitive_Identifiers => True,
                                        others => False]);
