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
  "!"                           { tok TokBang }
  "="                           { tok TokAssign }
  "{"                           { tok TokBraceOpen }
  "}"                           { tok TokBraceClose }
  "("                           { tok TokParenOpen }
  ")"                           { tok TokParenClose }
  ","                           { tok TokComma }
  ":"                           { tok TokColon }
  "::"                          { tok TokDoubleColon }
  "+"                           { tok (TokArithOp AoAdd) }
  "-"                           { tok (TokArithOp AoSub) }
  "*"                           { tok (TokArithOp AoMul) }
  "/"                           { tok (TokArithOp AoDiv) }
  "%"                           { tok (TokArithOp AoRem) }
  "+="                          { tok (TokArithAssign AoAdd) }
  "-="                          { tok (TokArithAssign AoSub) }
  "*="                          { tok (TokArithAssign AoMul) }
  "/="                          { tok (TokArithAssign AoDiv) }
  "%="                          { tok (TokArithAssign AoRem) }
  "=="                          { tok (TokCmpOp CoEQ) }
  "!="                          { tok (TokCmpOp CoNE) }
  "<"                           { tok (TokCmpOp CoLT) }
  "<="                          { tok (TokCmpOp CoLE) }
  ">"                           { tok (TokCmpOp CoGT) }
  ">="                          { tok (TokCmpOp CoGE) }
  @decimal                      { tokF (TokLitInt    . read . unpack . decodeUtf8) }
  @floating                     { tokF (TokLitDouble . read . unpack . decodeUtf8) }
  \" (\\.|[^\\\"])* \"          { string }
  @dataid                       { tokF (TokIdData . IdData . toStrict . decodeUtf8) }
  @varid                        { tokF (TokIdVar  . IdVar  . toStrict . decodeUtf8) }
  \' @varid                     { tokF (TokLabel  . Label  . toStrict . decodeUtf8 . tailEx) }

{
type Action = LByteString -> Int64 -> AlexInput -> P Token

tokF :: (LByteString -> TokenClass) -> Action
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
