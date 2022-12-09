open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let read = function
    | [ "L"; x ] -> ((-1, 0), int_of_string x)
    | [ "R"; x ] -> ((1, 0), int_of_string x)
    | [ "U"; x ] -> ((0, 1), int_of_string x)
    | [ "D"; x ] -> ((0, -1), int_of_string x)
    | _ -> failwith "parse narobe"

  let move (dx, dy) (h1, h2) (t1, t2) lst =
    let h, k = (h1 + dx, h2 + dy) in
    let test = max (abs (h - t1)) (abs (k - t2)) in
    if test <= 1 then ((h, k), (t1, t2), lst)
    else
      match (h - t1, k - t2) with
      | x, y when abs x = 2 && abs y = 2 -> 
          let nt1, nt2 = (t1 + dx, t2 + dy) in
          ((h, k), (nt1, nt2), (nt1, nt2) :: lst)
      | x, y when abs x = 2 ->
          let nt1, nt2 = (t1 + (x / 2), t2 + y) in
          ((h, k), (nt1, nt2), (nt1, nt2) :: lst)
      | x, y when abs y = 2 ->
          let nt1, nt2 = (t1 + x, t2 + (y / 2)) in
          ((h, k), (nt1, nt2), (nt1, nt2) :: lst)
      | _ -> failwith "move narobe"

  let rec multimove step head tail lst = function
    | 0 -> (head, tail, lst)
    | tms ->
        let new_head, new_tail, new_lst = move step head tail lst in
        multimove step new_head new_tail new_lst (tms - 1)

  let naloga1 data =
    let lines = List.map (String.split_on_char ' ') (List.lines data) in
    let rec naloga1_aux head tail lst = function
      | [] -> string_of_int (List.length (List.delete_duplicate lst))
      | x :: xs ->
          let step, times = read x in
          let new_head, new_tail, new_lst =
            multimove step head tail lst times
          in
          naloga1_aux new_head new_tail new_lst xs
    in
    naloga1_aux (0, 0) (0, 0) [ (0, 0) ] lines

  let naloga2 data _part1 = 
    let lines = List.map (String.split_on_char ' ') (List.lines data) in
    let rec naloga2_aux head tail lst = function
      | [] -> string_of_int (List.length (List.delete_duplicate lst))
      | x :: xs -> 
          let step, times = read x in
          let new_head, new_tail, new_list = multimove step head tail lst
    in naloga2_aux (0, 0) (List.init 9 (fun _ -> (0, 0))) [(0, 0)] lines
    ""
end
