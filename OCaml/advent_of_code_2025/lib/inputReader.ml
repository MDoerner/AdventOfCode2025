let read_day_input n = 
  let filename = "Day" ^ string_of_int(n) ^ ".txt" in
  let path = "inputs/" ^ filename in
  In_channel.with_open_bin path In_channel.input_all