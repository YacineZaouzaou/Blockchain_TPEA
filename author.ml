open Utils
    
let port = ref 12345
let addr = ref local_host

let my_pk : public_key option ref = ref None
let my_sk : private_key option ref = ref None

let letters_bag = ref [| |]

let last_word = ref 0
let head_blockchain = ref (hash256 "")

let send_letter letters_bag period =
  let ind = Random.int (Array.length letters_bag) in
  let c = letters_bag.(ind) in
  let letter = Yojson.Safe.to_string (`Assoc [
      ("letter" , `String (Char.escaped c));
      ("period" , `Int period);
      ("author", `String (get_pk_string !my_pk));
      ("head" , `String !head_blockchain);
      ("signature" , `String sign_default)
    ])
  in
  print_string ("envoi : " ^ Char.escaped c); print_newline ();
  flush stdout;
  send_message ("{\"inject_letter\":" ^ letter ^ "}")

let main () =
  init my_pk my_sk;
  let (is_next_mess, letters_json) = open_connection_response !addr !port !my_pk in
  if is_next_mess then
    failwith "TODO : is_next_mess"
  else begin
    letters_bag := Array.of_list (get_letters_bag letters_json);
    (*Printf.printf ("%d\n") (Array.length !letters_bag); *) 
    Unix.sleep 1;
  end;

  let period = ref 0 in
  while (!period < nb_tour_max) do
    try

      if !period = 0 then
          send_letter !letters_bag !period
        
      else
        begin
          Unix.sleep 1;
          
          (* Récupérer les mots construits à la période après le dernier mot trouvé *)
          let (is_next, wordpool_since) = get_wordpool_since !last_word in
          let wordpool = wordpool_since.wordpool in
          (*print_string "*********\n";
          print_wordpool wordpool;
            print_string "---\n";*)
          if is_next
          then raise Break;
          
          (* Sélectionner le meilleur mot, par concensus *)  
          let bw = Consensus.best_word wordpool.words in 
          print_string "Meilleur mot choisi : "; print_string (word_to_string bw);
          flush stdout;

          (* Enregistrer le mot : head de blockchain *)
          head_blockchain := hash256 !head_blockchain;
          last_word := !last_word +1;
          (* TODO : signature *)
          
          (* Enregistrer le score *)
          Consensus.record_score bw;
          
          (* Conclure le tour en envoyant une lettre*)
          send_letter !letters_bag !period;
        end;

      (* Attendre le tour suivant *)
      let is_next_mess = ref false in

      while not !is_next_mess do
        let (b, _) = comp_rep () in
        is_next_mess := b
      done;
      period := !period +1

    with Break ->
      period := !period + 1
  done;

  Consensus.print_score() ; flush stdout


let _ =
  (* port par défaut : 12345 
     addr par défaut : localhost *)
  let args = Sys.argv in
  let nb_args = Array.length args in
  let rec record_args i = match i with
      _ when i >= nb_args -> ()
    | _ when i+1 < nb_args && args.(i) = "-port" ->
      port := int_of_string args.(i+1); record_args (i+2)
    | _ when i+1 < nb_args && args.(i) = "-addr" ->
      addr := Unix.inet_addr_of_string args.(i+1); record_args (i+2)
    | _ -> failwith "argument must be [-port X] or [-addr X]"
  in record_args 1;

  main ()

