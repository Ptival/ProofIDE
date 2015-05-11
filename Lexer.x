{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  ($white|\160)+             ;
  let                        { \s -> TokenLet }
  in                         { \s -> TokenIn }
  $digit+                    { \s -> TokenInt (read s) }
  \(                         { \s -> TokenLParen }
  \)                         { \s -> TokenRParen }
  (\→|\-\>)                  { \s -> TokenArrow }
  (\λ|\\)                    { \s -> TokenLambda }
  \:                         { \s -> TokenColon }
  \@                         { \s -> TokenAnnot }
  \=                         { \s -> TokenEq }
  \_                         { \s -> TokenUnderscore }
  \?                         { \s -> TokenHole }
  "Type"                     { \s -> TokenType }
  (~$white # [\( \) \λ \ ])+ { \s -> TokenSym s }

{

data Token
  = TokenSym String
  | TokenLParen
  | TokenRParen
  | TokenArrow
  | TokenLambda
  | TokenColon
  | TokenAnnot
  | TokenLet
  | TokenIn
  | TokenEq
  | TokenUnderscore
  | TokenHole
  | TokenType
  | TokenInt Int
  deriving (Eq,Show)

scanTokens = alexScanTokens

}
