open Solvers.Signature
open Utils.List_utils

module Solver : Solver = struct
  type directory = {
    name : string;
    uberdir : directory option;
    subdirs : directory list ref;
    files : (int * string) list ref;
  }

  let cd global current = function
    | ".." -> (
        match current.uberdir with
        | None -> failwith "narobe1"
        | Some dir -> dir )
    | "/" -> global
    | d -> List.find (fun x -> x.name = d) !(current.subdirs)

  let ls lst crnt =
    let rec ls_aux acc1 acc2 = function
      | [] -> (acc1, acc2, [])
      | x :: xs -> (
          let instructions = String.split_on_char ' ' x in
          match instructions with
          | "dir" :: nm :: _ ->
              ls_aux
                ( {
                    name = nm;
                    uberdir = Some crnt;
                    subdirs = { contents = [] };
                    files = { contents = [] };
                  }
                :: acc1 )
                acc2 xs
          | [ i; name ] -> ls_aux acc1 ((int_of_string i, name) :: acc2) xs
          | _ -> (acc1, acc2, x :: xs) )
    in
    ls_aux [] [] lst

  let rec size dir =
    let rec size_subdir acc = function
      | [] -> acc
      | x :: xs -> size_subdir (acc + size x) xs
    in
    let rec size_files acc = function
      | [] -> acc
      | (s, _) :: other -> size_files (acc + s) other
    in
    size_subdir 0 !(dir.subdirs) + size_files 0 !(dir.files)

  let build_global lst =
    let rec do_it global current = function
      | [] -> global
      | x :: xs -> (
          let instructions = String.split_on_char ' ' x in
          match instructions with
          | "$" :: "cd" :: b :: _ -> do_it global (cd global current b) xs
          | "$" :: "ls" :: _ ->
              let dirs, files, other = ls xs current in
              let () = current.subdirs := dirs @ !(current.subdirs) in
              let () = current.files := files @ !(current.files) in
              do_it global current other
          | _ -> failwith "narobe2" )
    in
    do_it
      {
        name = "/";
        uberdir = None;
        subdirs = { contents = [] };
        files = { contents = [] };
      }
      {
        name = "/";
        uberdir = None;
        subdirs = { contents = [] };
        files = { contents = [] };
      }
      lst

  let rec sizes dir =
    match !(dir.subdirs) with
    | [] -> [ size dir ]
    | xs -> size dir :: List.flatten (List.map sizes xs)

  let naloga1 data =
    let lines = List.lines data in
    let global = build_global lines in
    let all = List.filter (fun x -> x <= 100000) (sizes global) in
    string_of_int (List.fold_left ( + ) 0 all)

  let naloga2 data _part1 =
    let lines = List.lines data in
    let global = build_global lines in
    let need = 30000000 - (70000000 - size global) in
    let all = List.filter (fun x -> x >= need) (sizes global) in
    string_of_int (List.fold_left min max_int all)
end
