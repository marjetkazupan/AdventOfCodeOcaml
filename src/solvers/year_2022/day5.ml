open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let rec move num from too =
    match from with
    | [] -> failwith "list too short"
    | x :: xs -> (
        match num with 1 -> (xs, x :: too) | a -> move (a - 1) xs (x :: too) )

  let move2 num from too =
    let new_from, change = move num from [] in
    let true_change = List.rev change in
    (new_from, true_change @ too)

  let get_nums str =
    let lst = String.split_on_char ' ' str in
    let rec pocisti acc = function
      | [] -> acc
      | x :: xs ->
          if Str.string_match (Str.regexp "[0-9]+") x 0 then
            pocisti (int_of_string x :: acc) xs
          else pocisti acc xs
    in
    pocisti [] lst

  let f = function [] -> "" | x :: _ -> String.make 1 x

  let make_storage lst =
    let sto = Array.make 9 [] in
    let rec make_storage_aux lst =
      match lst with
      | [] -> sto
      | x :: xs ->
          let rec modify acc a str =
            match str.[1] with
            | ' ' ->
                if String.length str < 4 then ()
                else
                  modify (acc + 1) a (String.sub str 4 (String.length str - 4))
            | chr ->
                let () = a.(acc) <- chr :: a.(acc) in
                if String.length str < 4 then ()
                else
                  modify (acc + 1) a (String.sub str 4 (String.length str - 4))
          in
          let () = modify 0 sto x in
          make_storage_aux xs
    in
    make_storage_aux lst

  let resi data premik =
    let lines = List.lines data in
    let lst11, lst22 = List.split_on_n lines 8 in
    let lst1 = List.rev lst11 in
    let _, lst2 = List.split_on_n lst22 2 in
    let lst_nums = List.map get_nums lst2 in
    let sto = make_storage lst1 in
    let rec naloga1_aux storage = function
      | [] -> Array.fold_left ( ^ ) "" (Array.map f sto)
      | [ t; f; m ] :: xs ->
          let new_from, new_too = premik m storage.(f - 1) storage.(t - 1) in
          let () = storage.(f - 1) <- new_from in
          let () = storage.(t - 1) <- new_too in
          naloga1_aux sto xs
      | _ -> failwith "narobe"
    in
    naloga1_aux sto lst_nums

  let naloga1 data = resi data move

  let naloga2 data _part1 = resi data move2
end
