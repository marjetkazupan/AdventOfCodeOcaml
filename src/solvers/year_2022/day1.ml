open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let rec naloga1_aux acc1 acc2 = function
      | [] -> string_of_int acc1
      | x :: xs ->
          if x = "" then naloga1_aux (max acc1 acc2) 0 xs
          else naloga1_aux acc1 (acc2 + int_of_string x) xs
    in
    naloga1_aux 0 0 lines

  let naloga2 data odgovor_prve_naloge = ""
end
