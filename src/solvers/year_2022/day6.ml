open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec check_duplicate = function
    | "" -> true
    | str ->
        let chr = str.[0] in
        let other = String.sub str 1 (String.length str - 1) in
        if String.contains other chr then false else check_duplicate other

  let naloga1 data =
    let rec naloga1_aux acc s =
      let bol = check_duplicate (String.sub s 0 4) in
      if bol then string_of_int (acc + 4)
      else naloga1_aux (acc + 1) (String.sub s 1 (String.length s - 1))
    in
    naloga1_aux 0 data

  let naloga2 data _part1 =
    let rec naloga2_aux acc s =
      let bol = check_duplicate (String.sub s 0 14) in
      if bol then string_of_int (acc + 14)
      else naloga2_aux (acc + 1) (String.sub s 1 (String.length s - 1))
    in
    naloga2_aux 0 data
end
