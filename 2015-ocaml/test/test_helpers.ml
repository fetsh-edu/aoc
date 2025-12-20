(** Common test utilities *)

open OUnit2

(** String assertion with printer - shows expected vs actual on failure *)
let assert_string_equal = assert_equal ~printer:Fun.id

(** Int assertion with printer - shows expected vs actual on failure *)
let assert_int_equal = assert_equal ~printer:string_of_int
