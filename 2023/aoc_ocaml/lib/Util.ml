let open_data day part =
  let file = Printf.sprintf "data/day%d_part%d" day part in
  In_channel.with_open_text file (fun ic -> In_channel.input_all ic)
