open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  let chunkify size lst =
    let rec aux chunk chunks n lst =
      match (n, lst) with
      | _, [] when chunk = [] -> List.rev chunks
      | _, [] -> List.rev (List.rev chunk :: chunks)
      | 0, _ :: _ -> aux [] (List.rev chunk :: chunks) size lst
      | _, x :: xs -> aux (x :: chunk) chunks (n - 1) xs
    in
    aux [] [] size lst

  let rec chars_in_common str1 str2 =
    let l = String.length str1 - 1 in
    let y0 = str1.[0] in
    if String.contains str2 y0 then y0
    else chars_in_common (String.sub str1 1 l) str2

  let ascii = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

  let naloga1 data =
    let lines = List.lines data in
    let rec naloga1_aux acc = function
      | [] -> string_of_int acc
      | x :: xs ->
          let l = String.length x / 2 in
          let part1, part2 = (String.sub x 0 l, String.sub x l l) in
          let dodaj = String.index ascii (chars_in_common part1 part2) + 1 in
          naloga1_aux (acc + dodaj) xs
    in
    naloga1_aux 0 lines

  let naloga2 data _part1 =
    let lines = chunkify 3 (List.lines data) in
    let rec badge str1 str2 str3 =
      let chr = chars_in_common str1 str2 in
      let start = 1 + String.index str1 chr in
      let str11 = String.sub str1 start (String.length str1 - start) in
      if String.contains str3 chr then chr else badge str11 str2 str3
    in
    let rec naloga2_aux acc = function
      | [] -> string_of_int acc
      | [ a; b; c ] :: xs ->
          naloga2_aux (acc + 1 + String.index ascii (badge a b c)) xs
      | _ -> failwith "narobe"
    in
    naloga2_aux 0 lines
end
