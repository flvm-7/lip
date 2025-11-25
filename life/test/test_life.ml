open Life.Ast
open Life.Main

let%test "test_ext_1" = 
  parse "ES2,3/B3" = Rule([2;3],[3])

let%test "test_ext_2" = 
  parse "E2,3/3" = Rule([2;3],[3])

let%test "test_ext_3" = 
  parse "E 2,3 / 3" = Rule([2;3],[3])

let%test "test_ext_4" = 
  parse "ES0..5,7..12/B" = Rule([0;1;2;3;4;5;7;8;9;10;11;12],[])
