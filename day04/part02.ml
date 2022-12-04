let range_of r =
  match String.split_on_char '-' r with
  | [ lo; hi ] -> (int_of_string lo, int_of_string hi)
  | _ -> failwith "invalid format 2"

let ranges =
  let rec read_ranges ranges_so_far =
    try
      let line = read_line () in
      match String.split_on_char ',' line with
      | [ range1; range2 ] ->
          read_ranges ((range_of range1, range_of range2) :: ranges_so_far)
      | _ -> failwith "invalid format"
    with End_of_file -> ranges_so_far
  in
  read_ranges []

let rec has_overlap ((lo1, hi1), (lo2, hi2)) = max lo1 lo2 <= min hi1 hi2
let int_of_bool b = if b then 1 else 0

let _ =
  ranges |> List.filter has_overlap |> List.length |> print_int;
  print_newline ()