exception Break

let nb_tour_max = 6

type public_key  = Hacl.public Hacl.Sign.key
type private_key = Hacl.secret Hacl.Sign.key


type letter  = { 
          letter    : string   ; 
          period    : int    ; 
          head      : string ;
          author    : string ;
          signature : string 
}

type letters = letter list

type word  = {
          word        : letters       ; 
          head        : string        ;
          politician  : string        ;
          signature   : string        
}

type wordpool = {
          period      : int           ;
          next_period : int           ;
          words       : word list    
        }

type wordpool_since = {
  since : int ;
  wordpool : wordpool
}

type letterpool = {
          period      : int           ;
          next_period : int           ;
          letters     : letters  
        }

type letterpool_since = {
  since : int;
  letterpool : letterpool
}

let empty_letter = {letter=""; period=0; head=""; author=""; signature=""}
let empty_letters = []
let empty_word = {word=empty_letters; head=""; politician=""; signature=""}
let empty_wordpool = {period=0; next_period=0; words=[]}
let empty_letterpool = {period=0; next_period=0; letters=empty_letters}
let empty_wordpool_since = {since=0; wordpool=empty_wordpool}
let empty_letterpool_since = {since=0; letterpool=empty_letterpool}

let word_to_string word =
  let rec print_rec letters = match letters with
    [] -> "\n"
    | l :: q -> l.letter ^ (print_rec q)
  in print_rec word.word

let out_chan : out_channel option ref = ref None
let in_chan : in_channel option ref = ref None

let local_host = (Unix.gethostbyname "localhost").h_addr_list.(0)

let sign_default = "8b6547447108e11c0092c95e460d70f367bc137d5f89c626642e1e5f2ceb6108043d4a080223b467bb810c52b5975960eea96a2203a877f32bbd6c4dac16ec07"




let print_letter (l : letter) = 
  print_string "print_letter\n";
  flush stdout;
  print_string (l.letter^"\n");
  print_int l.period;
  print_string ("\n"^l.head^"\n"^l.author^"\n"^l.signature^"\n")

let print_letterpool (lp : letterpool)  = 
  print_string "print_letter\n";
  flush stdout;
  print_int lp.period;
  print_string "\n";
  print_int lp.next_period;
  print_string "\n";
  List.iter print_letter lp.letters

let print_word (w : word) = 
  print_string "[\n";
  List.iter print_letter w.word;
  print_string ("\n"^w.head^"\n"^w.politician^"\n"^w.signature^"\n")


let print_wordpool (wp : wordpool)  = 
  print_int wp.period;
  print_string "\n";
  print_int wp.next_period;
  print_string "\n";
  List.iter print_word wp.words



  let init my_pk my_sk = 
  let (pk , sk) = Hacl.Sign.keypair () in 
    my_pk := Some pk;
    my_sk := Some sk

let gen_keys () = Hacl.Sign.keypair ()

let bytes_of_int i =
  let ibuf = Bytes.create 8 in
  Bytes.set_int64_be ibuf 0 (Int64.of_int i);
  ibuf

let bytes_to_int ibuf =
  Bytes.get_int64_be ibuf 0 |>
    Int64.to_int
  
let string_of_hex str =
  match str with (`Hex value) -> value

let get_pk_string my_pk = match my_pk with
    None -> failwith "no public key"
  | Some _ -> (Option.get my_pk |> Hacl.Sign.unsafe_to_bytes |> Hex.of_bigstring |> string_of_hex )
    
let read_size () = 
  let buf = Bytes.create 8 in 
    let _ = input (Option.get !in_chan) buf 0 8 in    
      bytes_to_int buf;;

let read_message size = 
  let buf = Bytes.create size in 
    let _  = input (Option.get !in_chan) buf 0 size in 
      buf;;

let send_message message =
  let message_byte = Bytes.unsafe_of_string message in 
    let size = Bytes.length message_byte |> bytes_of_int in 
      output_bytes (Option.get !out_chan) (Bytes.cat size message_byte);
      flush (Option.get !out_chan)

let compute_next_turn value =
  let t = Yojson.Basic.Util.to_int value in 
    Printf.printf "passing to new turn, turn number : %d\n" t;
    flush stdout



let letter_to_json letter = 
  let letter_json = `Assoc [ ("letter" , `String letter.letter); 
                              ("period" , `Int letter.period);
                              ("head" , `String letter.head);
                              ("author" , `String letter.author);
                              ("signature" , `String letter.signature)]
  in 
    letter_json

let letters_to_json letters = 
  List.map letter_to_json letters

let word_to_json (word : word) = 
  let word_json = `Assoc [ ("word" , `List (letters_to_json word.word)); 
            ("head" , `String word.head); 
            ("politician" , `String word.politician);
            ("signature" , `String word.signature)
  ] in 
  word_json

let is_next_turn message =
  let next_turn = "{\"next_turn\":" in
  let rec parcours message i = match i with
      _ when i >= String.length next_turn -> true
    | _ when i >= String.length message -> false
    | _ when message.[i] = next_turn.[i] -> parcours message (i+1)
    | _ -> false
  in parcours message

let comp_rep () =
  let size = read_size () in 
  let message = read_message size in
  let str_message = Bytes.to_string message in 
  (is_next_turn str_message 0, str_message)

let read_response () =
  let size = read_size () in 
  let message = read_message size in 
  (*print_string (Bytes.to_string message);
  print_string "\n";
    flush stdout;*)

  let str_message = Bytes.to_string message in 
  (is_next_turn str_message 0, str_message)

(* let do_wordpool value = 
  let  *)

let to_letter json_letter = 
  let letter = List.nth (Yojson.Basic.Util.to_list json_letter) 1 in  
  let lett = letter |> Yojson.Basic.Util.member "letter" |> Yojson.Basic.Util.to_string in 
  let per = letter |> Yojson.Basic.Util.member "period" |> Yojson.Basic.Util.to_int in 
  let head = letter |> Yojson.Basic.Util.member "head" |> Yojson.Basic.Util.to_string in
  let author = letter |> Yojson.Basic.Util.member "author" |> Yojson.Basic.Util.to_string in
  let sign = letter |> Yojson.Basic.Util.member "signature" |> Yojson.Basic.Util.to_string in 
    { letter = lett ; period = per ; head = head ; author = author ; signature = sign}

let to_letter_tranq json_letter = 
  let letter = json_letter in  
  let lett = letter |> Yojson.Basic.Util.member "letter" |> Yojson.Basic.Util.to_string in 
  let per = letter |> Yojson.Basic.Util.member "period" |> Yojson.Basic.Util.to_int in 
  let head = letter |> Yojson.Basic.Util.member "head" |> Yojson.Basic.Util.to_string in
  let author = letter |> Yojson.Basic.Util.member "author" |> Yojson.Basic.Util.to_string in
  let sign = letter |> Yojson.Basic.Util.member "signature" |> Yojson.Basic.Util.to_string in 
    { letter = lett ; period = per ; head = head ; author = author ; signature = sign}
    


let to_letter_list json_list = 
  let list_json_obj = json_list in 
    List.map to_letter list_json_obj

let read_letterpool value = 
  let cur_per  = value |> Yojson.Basic.Util.member "current_period" |> Yojson.Basic.Util.to_int in 
    let nex_per = value |> Yojson.Basic.Util.member "next_period" |> Yojson.Basic.Util.to_int in 
      let lett = value |> Yojson.Basic.Util.member "letters" |> Yojson.Basic.Util.to_list |> to_letter_list in 
        {period = cur_per ; next_period = nex_per ; letters = lett}

let compute_response_letterpool () = 
  let (is_next_mess, message) = read_response() in
  if is_next_mess then
    (is_next_mess, empty_letterpool)
  else
    let values = Yojson.Basic.from_string message in 
        (is_next_mess, read_letterpool (Yojson.Basic.Util.member "full_letterpool" values))

let compute_response () = 
  let (_, message) = read_response() in
    let values = Yojson.Basic.Util.to_assoc (Yojson.Basic.from_string message) in 
      let member , value = List.nth values 0 in
        match member with 
          "letters_bag"         -> print_string "no thing to do for politician\n" ; flush stdout
        | "next_turn"           -> compute_next_turn value
        | "full_letterpool"     -> print_string "to be done\n"  ; flush stdout
        | _                     -> print_string "to be done \n" ; flush stdout 


let to_word (json_word : Yojson.Basic.t) = 
  let w = json_word in 
  let word = w |> Yojson.Basic.Util.member "word" |> Yojson.Basic.Util.to_list |> List.map to_letter_tranq in 
  let head = w |> Yojson.Basic.Util.member "head" |> Yojson.Basic.Util.to_string in 
  let polit = w |> Yojson.Basic.Util.member "politician" |> Yojson.Basic.Util.to_string in 
  let sign = w |> Yojson.Basic.Util.member "signature" |> Yojson.Basic.Util.to_string in 
    {word = word ; head = head ; politician = polit ; signature = sign} 



(* les message reçus ne suvent pas la spec *)
let to_word_tranq json_word = 
  let value = Yojson.Basic.Util.to_list json_word in 
    to_word (List.nth value 1)

let get_full_letterpool () =
  let message = Yojson.Basic.pretty_to_string (`Assoc [( "get_full_letterpool" , `Null) ]) in 
      send_message message;
      (* let response = read_response () in  *)
      compute_response_letterpool ()

let read_wordpool value = 
  let cur_per  = value |> Yojson.Basic.Util.member "current_period" |> Yojson.Basic.Util.to_int in 
    let nex_per = value |> Yojson.Basic.Util.member "next_period" |> Yojson.Basic.Util.to_int in 
      let wor = value |> Yojson.Basic.Util.member "words" |> Yojson.Basic.Util.to_list |> List.map to_word_tranq in 
  {period = cur_per ; next_period = nex_per ; words = wor}

let read_wordpool_since value =
  let sc =  value |> Yojson.Basic.Util.member "since" |> Yojson.Basic.Util.to_int in 
  let wdpl  = value |> Yojson.Basic.Util.member "wordpool" |> read_wordpool in
  {since = sc ; wordpool = wdpl}

let read_letterpool_since value =
  let sc =  value |> Yojson.Basic.Util.member "since" |> Yojson.Basic.Util.to_int in 
  let ltp  = value |> Yojson.Basic.Util.member "letterpool" |> read_letterpool in
  {since = sc ; letterpool = ltp}

let compute_response_wordpool () = 
  let (is_next_mess, message) = read_response() in (*Printf.printf ("%s\n") message;*)
  if is_next_mess then
    (is_next_mess, empty_wordpool)
  else
    let values = Yojson.Basic.from_string message in 
    (is_next_mess, read_wordpool (Yojson.Basic.Util.member "full_wordpool" values))

let compute_response_wordpool_since () =
  let (is_next_mess, message) = read_response() in (*Printf.printf ("%s\n") message;*)
  if is_next_mess then
    (is_next_mess, empty_wordpool_since)
  else
    let values = Yojson.Basic.from_string message in 
    (is_next_mess, read_wordpool_since (Yojson.Basic.Util.member "diff_wordpool" values))

let compute_response_letterpool_since () =
  let (is_next_mess, message) = read_response() in (*Printf.printf ("%s\n") message;*)
  if is_next_mess then
    (is_next_mess, empty_letterpool_since)
  else
    let values = Yojson.Basic.from_string message in 
    (is_next_mess, read_letterpool_since (Yojson.Basic.Util.member "diff_letterpool" values))
    
let get_full_wordpool () = 
  let message = "{\"get_full_wordpool\" : null}" in 
    send_message message;
    compute_response_wordpool ()

let get_wordpool_since period =
  let message =  Yojson.Safe.to_string (`Assoc [
      ("get_wordpool_since" , `Int period)
    ])
  in (*print_string message ; print_newline();*)
  send_message message;
  compute_response_wordpool_since ()

let get_letterpool_since period =
  let message =  Yojson.Safe.to_string (`Assoc [
      ("get_letterpool_since" , `Int period)
    ])
  in (*print_string message ; print_newline();*)
  send_message message;
  compute_response_letterpool_since ()

(* let compute_next_request request = 
  match request with 
      "get_full_letterpool"     -> get_full_letterpool ()
    | "get_full_wordpool"       -> get_full_wordpool ()
    | _                         -> print_string "unknown request\n"; flush stdout *)


let register my_pk = 
  let message = Yojson.Safe.to_string (`Assoc [ ("register" , `String (get_pk_string my_pk))]) in 
    send_message message

let open_connection addr port = 
  let sock_addr = Unix.ADDR_INET (addr, port) in 
    let domain = Unix.domain_of_sockaddr sock_addr in 
      let sock = Unix.socket domain Unix.SOCK_STREAM 0 in 
        try Unix.connect sock sock_addr;
          (Unix.in_channel_of_descr sock , Unix.out_channel_of_descr sock);
        with exn -> Unix.close sock; raise exn;;

let open_connection_main addr port = 
  let inchan , outchan = open_connection addr port in 
    in_chan := Some inchan;
    out_chan := Some outchan
    (*register my_pk;
      compute_response ()*)

let open_connection_response addr port my_pk =
  let inchan , outchan = open_connection addr port in 
  in_chan := Some inchan;
  out_chan := Some outchan;
  register my_pk;
  read_response()
  
let my_printer bs = 
  let rec my_printer_rec i l b = 
    match i with 
      v when v < l  -> Printf.printf "%d\n" (Char.code (Bigstring.get b i)); my_printer_rec (i+1) l b;
    | _ -> print_string "\n"; flush stdout;
  in my_printer_rec 0 (Bigstring.length bs) bs


(* convertit json letters_bag en liste de char *)
let get_letters_bag letters_json =
  let rec record_table str i = match str.[i] with
      ']' -> []
    | '"' -> record_table str (i+1)
    | ',' -> record_table str (i+1)
    | _ -> str.[i] :: (record_table str (i+1))
  in
  let rec init_table str i = match str.[i] with
      '[' -> record_table str (i+1)
    | _ -> init_table str (i+1)
  in
  init_table letters_json 0



let hash256 msg = 
  let msgbs = Bigstring.of_string msg in 
  let hashbs = Hacl.Hash.SHA256.digest msgbs in 
  let hash = string_of_hex (Hex.of_bigstring hashbs) in 
  hash
