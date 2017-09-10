{
{-# LANGUAGE NamedFieldPuns #-}

module Language.Soi.Internal.Lexer
  ( lexToken
  )
where

import           ClassyPrelude                    hiding (undefined)

import           Text.Read                        (read, readEither)

import           Language.Soi.Internal.ParseMonad
}

$digit       = [0-9]
$lower_alpha = [a-z]
$upper_alpha = [A-Z]
$alpha       = [$lower_alpha $upper_alpha]
$alphanum    = [$alpha $digit]

@decimal  = $digit+
@exponent = [eE] [\-\+]? @decimal
@floating = @decimal \. @decimal @exponent? | @decimal @exponent

@varid  = $lower_alpha $alphanum*
@dataid = $upper_alpha $alphanum*

tokens :-
  $white                        ;
  (("//" .*)? \n)+              ;
  "data"                        { tok TokData }
  "impl"                        { tok TokImpl }
  "def"                         { tok TokDef }
  "val"                         { tok TokVal }
  "var"                         { tok TokVar }
  "self"                        { tok TokSelf }
  "if"                          { tok TokIf }
  "then"                        { tok TokThen }
  "else"                        { tok TokElse }
  "loop"                        { tok TokLoop }
  "break"                       { tok TokBreak }
  "continue"                    { tok TokContinue }
  ";"                           { tok TokSemi }
  "."                           { tok TokDot }
  "="                           { tok TokAssign }
  "{"                           { tok TokBraceOpen }
  "}"                           { tok TokBraceClose }
  "("                           { tok TokParenOpen }
  ")"                           { tok TokParenClose }
  ","                           { tok TokComma }
  ":"                           { tok TokColon }
  "::"                          { tok TokDoubleColon }
  "!"                           { tok TokBang }
  "~"                           { tok TokTilde }
  "+"                           { tok (TokBinOp (Bao (Bano BanoAdd))) }
  "-"                           { tok (TokBinOp (Bao (Bano BanoSub))) }
  "*"                           { tok (TokBinOp (Bao (Bano BanoMul))) }
  "/"                           { tok (TokBinOp (Bao (Bano BanoDiv))) }
  "%"                           { tok (TokBinOp (Bao (Bano BanoRem))) }
  "&"                           { tok (TokBinOp (Bao (Babo BaboAnd))) }
  "|"                           { tok (TokBinOp (Bao (Babo BaboOr ))) }
  "^"                           { tok (TokBinOp (Bao (Babo BaboXor))) }
  "<<"                          { tok (TokBinOp (Bao (Baso BasoShl))) }
  ">>"                          { tok (TokBinOp (Bao (Baso BasoShr))) }
  "&&"                          { tok (TokBinOp (Blo BloAnd)) }
  "||"                          { tok (TokBinOp (Blo BloOr )) }
  "=="                          { tok (TokBinOp (Bco (Bceo BceoEQ))) }
  "!="                          { tok (TokBinOp (Bco (Bceo BceoNE))) }
  "<"                           { tok (TokBinOp (Bco (Bcoo BcooLT))) }
  "<="                          { tok (TokBinOp (Bco (Bcoo BcooLE))) }
  ">"                           { tok (TokBinOp (Bco (Bcoo BcooGT))) }
  ">="                          { tok (TokBinOp (Bco (Bcoo BcooGE))) }
  "+="                          { tok (TokArithAssign (Bano BanoAdd)) }
  "-="                          { tok (TokArithAssign (Bano BanoSub)) }
  "*="                          { tok (TokArithAssign (Bano BanoMul)) }
  "/="                          { tok (TokArithAssign (Bano BanoDiv)) }
  "%="                          { tok (TokArithAssign (Bano BanoRem)) }
  "&="                          { tok (TokArithAssign (Babo BaboAnd)) }
  "|="                          { tok (TokArithAssign (Babo BaboOr )) }
  "^="                          { tok (TokArithAssign (Babo BaboXor)) }
  "<<="                         { tok (TokArithAssign (Baso BasoShl)) }
  ">>="                         { tok (TokArithAssign (Baso BasoShr)) }
  "true"                        { tok (TokLitBool True) }
  "false"                       { tok (TokLitBool False) }
  @decimal                      { tokF (TokLitInt    . read . unpack . decodeUtf8) }
  @floating                     { tokF (TokLitDouble . read . unpack . decodeUtf8) }
  \" (\\.|[^\\\"])* \"          { string }
  @dataid                       { tokF (TokIdData . IdData . decodeUtf8) }
  @varid                        { tokF (TokIdVar  . IdVar  . decodeUtf8) }
  \' @varid                     { tokF (TokLabel  . Label  . decodeUtf8 . tailEx) }

{
type Action = ByteString -> Int -> AlexInput -> P Token

tokF :: (ByteString -> TokenClass) -> Action
tokF f buf1 len inp2@(AI loc2 _ _) =
  do
    setInput inp2
    return (Token loc2 (f (take len buf1)))

tok :: TokenClass -> Action
tok = tokF . const

lexToken :: P Token
lexToken =
  do
    inp1@(AI _ buf1 pos1) <- getInput
    case alexScan inp1 0 of
      AlexEOF -> do
        (AI loc2 _ _) <- getInput
        return (Token loc2 TokEof)
      AlexError _ -> do
        lexFail Nothing
      AlexSkip inp2 _ -> do
        setInput inp2
        lexToken
      AlexToken inp2@(AI _ _ pos2) _ act -> do
        act buf1 (pos2 - pos1) inp2

string :: Action
string buf1 len inp2@(AI loc2 _ _) =
  do
    str <- unescString . unpack . decodeUtf8 . take len $ buf1
    setInput inp2
    return (Token loc2 (TokLitString (pack str)))
  where
    unescString :: String -> P String
    unescString s =
      case readEither s of
        Left  _ -> lexFail (Just ("Bad string constant: " ++ s))
        Right r -> return r

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "left-contexts are not supported"

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AI {loc,buf,pos}) =
  case uncons buf of
    Nothing       -> Nothing
    Just (b,buf') -> Just (b, AI (advanceSrcLoc loc (toEnum (fromIntegral b))) buf' (pos+1))

-- Alex uses undefined for `monadUserState` when there is no monad user state
-- but the ClassyPrelude's `undefined` is deprecated, so we're just gonna
-- sneakily redefine it here
undefined = error "no monadUserState"
}
