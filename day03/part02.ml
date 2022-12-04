let explode s = List.init (String.length s) (String.get s)

let char_to_priority c =
  Char.code c - (if c <= 'Z' then Char.code 'A' - 26 else Char.code 'a') + 1

let rucksacks =
  let rec read_rucksacks rucksacks_so_far =
    try
      let line1 = read_line () |> explode |> List.map char_to_priority in
      let line2 = read_line () |> explode |> List.map char_to_priority in
      let line3 = read_line () |> explode |> List.map char_to_priority in
      read_rucksacks ((line1, line2, line3) :: rucksacks_so_far)
    with End_of_file -> rucksacks_so_far
  in
  read_rucksacks []

let common_elements (l1, l2) =
  let rec find_commons so_far l1 l2 =
    match (List.sort_uniq compare l1, List.sort_uniq compare l2) with
    | [], _ | _, [] -> so_far
    | h1 :: t1, h2 :: t2 when h1 = h2 -> find_commons (h1 :: so_far) t1 t2
    | h1 :: t1, h2 :: t2 when h1 < h2 -> find_commons so_far t1 l2
    | h1 :: t1, h2 :: t2 -> find_commons so_far l1 t2
  in
  find_commons [] l1 l2

let common_3 (l1, l2, l3) = common_elements (common_elements (l1, l2), l3)

let _ =
  rucksacks |> List.map common_3 |> List.flatten |> List.fold_left ( + ) 0
  |> print_int;
  print_newline ()
