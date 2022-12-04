let elf_data =
  let rec read_elf_data data_so_far current_item =
    try
      let line = read_line () in
      if line = "" then read_elf_data (current_item :: data_so_far) []
      else read_elf_data data_so_far (int_of_string line :: current_item)
    with End_of_file -> current_item :: data_so_far
  in
  read_elf_data [] []

let _ =
  elf_data
  |> List.map (List.fold_left ( + ) 0)
  |> List.fold_left max 0 |> print_int;
  print_newline ()
