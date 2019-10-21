open Unix

let s = socket PF_INET SOCK_STREAM 0

let host = gethostbyname "localhost"
let ip_addr = host.h_addr_list.(0)
let port = 12345
let addr = ADDR_INET (ip_addr, port)

let () = connect s addr

let out_ch = out_channel_of_descr s
let in_ch = in_channel_of_descr s

let size = "0010100000000000000000000000000000000000000000000000000000000000"

let () = output_string out_ch (size^"{register:123456789}")
let () = flush out_ch
  
let () =
  if Sys.big_endian
  then begin print_string "Big-endian"; print_newline(); end
  else  begin print_string "No big-endian"; print_newline(); end


let () = print_string "Fin!" ; print_newline()

