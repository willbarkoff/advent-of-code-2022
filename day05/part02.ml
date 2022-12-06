exception Not_crate_line

let explode s = List.init (String.length s) (String.get s)

let parse_crate_line ln =
  ln |> explode
  |> List.filteri (fun i _ -> (i mod 4) - 1 = 0)
  |> List.map (fun c ->
         if c = ' ' then None
         else if c > '0' && c < '9' then raise Not_crate_line
         else Some c)

let crate_lines =
  let rec aux lines_so_far =
    try
      let line = read_line () in
      aux (parse_crate_line line :: lines_so_far)
    with Not_crate_line -> lines_so_far
  in
  aux []

let crates_arr =
  Array.init (List.length @@ List.hd @@ crate_lines) (fun _ -> [])

let _ =
  List.iter
    (List.iteri (fun i -> function
       | None -> () | Some chr -> crates_arr.(i) <- chr :: crates_arr.(i)))
    crate_lines

(* [(x, y, z)] means move x crates from y to z *)
type instruction = int * int * int

let parse_char_int c = String.make 1 c |> int_of_string

let instructions =
  let rec aux acc =
    try
      let line = read_line () in
      if String.length line = 0 then aux acc
      else
        match
          List.map int_of_string (Str.split (Str.regexp "[^0-9]+") line)
        with
        | [ a; b; c ] -> aux ((a, b, c) :: acc)
        | _ -> failwith "BAD"
    with End_of_file -> acc
  in
  aux [] |> List.rev

let print_instruction (a, b, c) = Printf.printf "move %d from %d to %d\n" a b c

let rec execute_instr (a, b, c) =
  if a = 0 then ()
  else (
    crates_arr.(c - 1) <-
      List.nth crates_arr.(b - 1) (a - 1) :: crates_arr.(c - 1);
    crates_arr.(b - 1) <-
      List.filteri (fun i _ -> i <> a - 1) crates_arr.(b - 1);
    execute_instr (a - 1, b, c))

let _ =
  List.iter execute_instr instructions;
  crates_arr |> Array.map List.hd |> Array.iter (Printf.printf "%c");
  print_endline ""
