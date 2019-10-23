open Unix

let s = socket PF_INET SOCK_STREAM 0

let host = gethostbyname "localhost"
let ip_addr = host.h_addr_list.(0)
let port = 12345
let addr = ADDR_INET (ip_addr, port)

let () = connect s addr

let out_ch = out_channel_of_descr s
let in_ch = in_channel_of_descr s

let bytes_of_int i =
  let ibuf = Bytes.create 8 in
  Bytes.set_int64_be ibuf 0 (Int64.of_int i);
  ibuf

let message = "{get_letterpool_since:0}"

let message = "{get_full_wordpool:null}"


(*let message = "{register:123456789zedzedfsq}"*)

let pack = Bytes.cat (bytes_of_int (String.length message)) (Bytes.of_string message)

let () = output_bytes out_ch pack
let () = flush out_ch

let () =

let i = ref 0 in

  while (!i) < 100 do

    let reponse = input_char in_ch in

    print_string (Char.escaped reponse);

    i := !i +1
  done;

print_newline()
