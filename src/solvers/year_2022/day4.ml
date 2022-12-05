open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let naloga1 data =
    let lines = List.lines data in
    let rec naloga1_aux acc = function
      | [] -> string_of_int acc
      | x :: xs ->
          let [ a; b ] = String.split_on_char ',' x in
          let [ a1; a2 ] =
            List.map int_of_string (String.split_on_char '-' a)
          in
          let [ b1; b2 ] =
            List.map int_of_string (String.split_on_char '-' b)
          in
          if (a1 <= b1 && a2 >= b2) || (a1 >= b1 && a2 <= b2) then
            naloga1_aux (acc + 1) xs
          else naloga1_aux acc xs
    in
    naloga1_aux 0 lines

  let naloga2 data _part1 =
    let lines = List.lines data in
    let rec naloga2_aux acc = function
      | [] -> string_of_int acc
      | x :: xs ->
          let [ a; b ] = String.split_on_char ',' x in
          let [ a1; a2 ] =
            List.map int_of_string (String.split_on_char '-' a)
          in
          let [ b1; b2 ] =
            List.map int_of_string (String.split_on_char '-' b)
          in
          if (a1 <= b2 && a2 >= b1) || (a1 >= b2 && a2 <= b1) then
            naloga2_aux (acc + 1) xs
          else naloga2_aux acc xs
    in
    naloga2_aux 0 lines
end
