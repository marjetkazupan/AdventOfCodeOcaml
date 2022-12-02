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

  let naloga2 data odgovor_prve_naloge =
    let lines = List.lines data in
    let rec naloga2_aux acc1_lst acc2 = function
      | [] -> (
          let lst =
            List.sort (fun a_int b_int -> -compare a_int b_int) acc1_lst
          in
          match lst with
          | x :: y :: z :: other -> string_of_int (x + y + z)
          | _ -> failwith "ni škratov" )
      | x :: xs ->
          if x = "" then naloga2_aux (acc2 :: acc1_lst) 0 xs
          else naloga2_aux acc1_lst (acc2 + int_of_string x) xs
    in
    naloga2_aux [] 0 lines
end
