open Toylexer.Token
open Toylexer.Main

let%test "test_frequencies_1" =
  lexer "x=y; x=x+1" |> frequency 3 = [(ID "x", 3); (ASSIGN, 2); (ID "y", 1)]

(* YOUR TESTS HERE *)

let%test "test_ATOK" =
  (lexer "G3n3vra" = [ATOK "G3n3vra"; EOF]) && 
  (lexer "s = Alpha" = [ID "s"; ASSIGN; ATOK "Alpha"; EOF]) &&
  (lexer "3 F" = [CONST "3"; ATOK "F"; EOF])

let%test "test_BTOK" =
  (lexer "eiou" = [BTOK "eiou"; EOF]) &&
  (lexer "a" = [BTOK "a"; EOF]) &&
  (lexer "io" = [BTOK "io"; EOF])

let%test "test_CTOK" = 
  (lexer "cel" = [CTOK "cel"; EOF]) && 
  (lexer "ra" = [CTOK "ra" ; EOF]) && 
  (lexer "lTTOTTL" = [CTOK "lTTOTTL"; EOF])

let%test "test_DTOK" = 
  (lexer "pi = 3.14; r = 2" = 
    [CTOK "pi"; ASSIGN; DTOK"3.14"; SEQ; ID "r"; ASSIGN; CONST "2"; EOF]) &&
  (lexer "c = -.3; a = -7. + c" = 
    [ID "c"; ASSIGN; DTOK "-.3"; SEQ; BTOK "a"; ASSIGN; DTOK "-7."; PLUS; ID "c"; EOF]) &&
  (lexer "(-.3 + 2) + (-3.4)" = 
    [LPAREN; DTOK"-.3"; PLUS; CONST "2"; RPAREN; PLUS; LPAREN; DTOK "-3.4"; RPAREN ; EOF])

let%test "test_ETOK" =
  (lexer "0xa1" = [ETOK "0xa1"; EOF]) && 
  (lexer "0XF3" = [ETOK "0XF3"; EOF]) && 
  (lexer "0Xff3" = [ETOK "0Xff3"; EOF])