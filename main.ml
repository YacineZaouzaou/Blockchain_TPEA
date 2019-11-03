open Utils

let port = ref 12345
let addr = ref local_host

let nb_authors = 5

let period = ref 0
let head_blockchain = ref (hash256 "")

let main () =
  let authors = Author.list_authors nb_authors !addr !port in
  (* Envoie de lettres : pas d'attente de réponse *)
  Author.inject_letter authors !period !head_blockchain;

  get_full_letterpool()


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
