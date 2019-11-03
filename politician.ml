open Utils

let my_pk : public_key option ref = ref None
let my_sk : private_key option ref = ref None
let turn : int ref = ref 0
(*let test_hash : string ref = ref "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"*)

let hash_ini = hash256 ""

let path_dict = "./dict_100000_1_10.txt"

let local_host = (Unix.gethostbyname "localhost").h_addr_list.(0)

let init_dict path =
  let dict = open_in path in
  let rec dict_lines () =
    try
      let line = input_line dict in
      line :: (dict_lines ())
    with End_of_file -> close_in dict; []
  in dict_lines()

 let shuffle d = begin
    Random.self_init ();
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
 end

let dict = shuffle (init_dict path_dict) 
           
let init my_pk my_sk = 
  let (pk , sk) = Hacl.Sign.keypair () in 
    my_pk := Some pk;
    my_sk := Some sk

let word_test = {word = [ { letter = "a"; period = 0;
head="e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
author="0b418daae4ca18c026d7f1d55237130cbdb9e874d98f7480f85f912c6470ab77";
signature="2909ad8ca59787d6421e71e4e9dc807cbbe120892fe9f0d7627d6a6be8746a099fad797884344faff10a892bd1c10bd351f911be05269a3a24f9c5bbace78409"};
{ letter="b"; period = 0;
head="e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
author="0b418daae4ca18c026d7f1d55237130cbdb9e874d98f7480f85f912c6470ab77";
signature="08efc0569047f34e6cda7410ae2c9aa4d8097438948bc8c3c671cd6b8d309433324ba2e32ecb0fdd2b7aa807a19d6c62957e0d6e9f60897286ff0d9f99bd3106"}
];
head="e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
politician="0b418daae4ca18c026d7f1d55237130cbdb9e874d98f7480f85f912c6470ab77";
signature="c7a41b5bfcec80d3780bfc5d3ff8c934f7c7f41b27956a8acb20aee066b406edc5d1cb26c42a1e491da85a97650b0d5854680582dcad3b2c99e2e04879769307"}

let inject_word word =
  send_message ("{\"inject_word\" : "^(Yojson.Basic.to_string (word_to_json word))^"}")

let filtre_by_hash letters hash = 
  let rec filtre_rec cons (lett : letters) ha = 
      match lett with 
        [] -> cons
        | h::t -> if String.equal h.head ha then let c = List.append [h] cons in filtre_rec c t ha else filtre_rec cons t ha;
    in
      filtre_rec [] letters hash

let is_in value liste = 
  let rec is_in_rec v l = 
    match l with 
      [] -> false
    | t::h -> if String.equal v t then true else is_in_rec v h
  in 
    is_in_rec value liste

let filtre_by_author letter = 
  let rec filtre_author authors cons lett = 
    match lett with 
      [] -> cons 
    | h::t -> if is_in h.author authors then 
                                          filtre_author authors cons t
                                        else 
                                          let aths = List.append [h.author] authors in 
                                            let c = List.append [h] cons in 
                                              filtre_author aths c t
  in
    filtre_author [] [] letter

let contains_letter letter letters = 
  let rec contains_letter_rec l ls = 
    match ls with 
        lst when List.length lst == 1 -> String.equal l ((List.nth lst 0).letter)
    |   h::lst  -> if String.equal l (h.letter) then true else contains_letter_rec l lst
    | [] -> false
  in contains_letter_rec letter letters

let not_equals lettre lt = 
  if String.equal lettre (lt.letter) then false else true

let remove_letter lt lettre = 
  List.filter (not_equals lettre) lt

let valid_word line letters = 
  let rec valid_word_rec ln lt = 
    match ln with
     s when String.length s == 1 -> contains_letter s lt
    | s when String.length s > 1 -> let letre = String.sub s 0 1 in
                                       if contains_letter letre lt then 
                                          let suite = String.sub s 1 ((String.length s) - 1) in 
                                            let remaind_letters = remove_letter lt letre in 
                                              valid_word_rec suite remaind_letters
                                        else
                                          false
    | _ -> print_string "Normaly never get this state of got it read function valid_word in file politician ---------------------\n";  flush stdout ; false   
  in valid_word_rec line letters

let rec melanger paquet acc =
  if List.length acc = 52 then acc
  else
    let element_tire = List.nth paquet (Random.int (List.length paquet)) in
    melanger (List.find_all (fun x -> x <> element_tire) paquet) (element_tire :: acc)


let rec read_dict dict letters = match dict with
    [] -> ""
  | mot :: q -> if valid_word mot letters then mot else read_dict q letters
    

let equal ch letter = 
  if String.equal ch letter.letter then true else false

let get_valid_letters hash =
  let (next ,  all_letters) = get_full_letterpool () in 
  if next  then
    begin
      print_string "nouveau tour : stop recherche mot\n";
      flush stdout;
      raise Break
    end
    else 
      let letters_by_hash = filtre_by_hash all_letters.letters hash in 
        filtre_by_author (shuffle letters_by_hash)

let compute_word word letter = 
  let rec compute_word_rec w l cw = 
    match w with  
      s when String.length s == 0 -> cw
    | s when String.length s > 0  -> let first = String.sub s 0 1 in 
                                      let tail = String.sub s 1 ((String.length s) - 1) in 
                                        let ncw = List.append cw [List.find (equal first) letter ] in
                                        let nl = remove_letter l first in 
                                          compute_word_rec tail nl ncw
    | _ -> print_string "Normaly never get this state of got it read function compute_word in file politician ---------------------\n";  flush stdout ; cw
  in compute_word_rec word letter []


let create_word word letters head = 
  let politician  = get_pk_string !my_pk in 
  let signature = sign_default in 
  let word = compute_word word letters in 
  { word = word ; head = head ; politician = politician ; signature = signature }
  
let find_word valid_letters hash = 
  let word = read_dict dict valid_letters in
  print_string "mot trouvé : ";
  print_string word;
  print_newline();
  flush stdout;
  if not (String.equal word "") then
    let word_to_inject = create_word word valid_letters hash in 
    inject_word word_to_inject

let hash_of_period period =
  let (is_next , ltp_since) = get_letterpool_since period in
  if is_next then
    raise Break;
  let ltp = ltp_since.letterpool in
  (List.hd ltp.letters).head
  

let main args = 
  init my_pk my_sk;
  
  let _ = match Array.length args with 
      2 -> open_connection_main local_host (int_of_string args.(1))
    | 3 -> open_connection_main (Unix.inet_addr_of_string args.(1)) (int_of_string args.(2)) 
    | _ -> print_string "error number of args \n"
  in

  (*for i = 0 to 10 do
    let mot = (List.nth dict i) in
    print_string (mot ^ " - taille : ");
    print_int (String.length mot);
    print_newline()
    done; *)
  
  let period = ref 0 in
  
  while (!period < nb_tour_max) do

    let valid_letters = ref [] in
    
    try
      while true do

        try
          let hash = if !period=0 then hash_ini else hash_of_period (!period-1) in
          let new_valid_letters = (get_valid_letters hash) in
          if (List.length new_valid_letters) != (List.length !valid_letters) then
            begin
              valid_letters := new_valid_letters;
              print_string "lettres valides : ";
              List.iter (fun l -> Printf.printf ("%s ") (l.letter)) !valid_letters;
              print_newline();
              flush stdout;
              find_word !valid_letters hash
            end
        with Break -> raise Break
           | _ -> ()

      done
    with Break -> period := !period +1
                             

  done
  
let _ = main Sys.argv
