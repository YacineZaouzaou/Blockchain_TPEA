open Utils

let score_table_auth = ref []
let score_table_pol = ref []

let score_lettre c = match c with
  'e' | 'a' | 'i' | 'n' | 'o' | 'r' | 's' | 't' | 'u' | 'l' -> 1
      | 'd' | 'm' | 'g' -> 2
      | 'b' | 'c' | 'p' -> 3
      | 'f' | 'h' | 'v' -> 4
      | 'j' | 'q' -> 8
      | 'k' | 'w' | 'x' | 'y' | 'z' -> 10
      | _ -> Printf.printf ("[warning] lettre sans score : %c\n") c; 0

let rec score word = match word with
    [] -> 0
  | l :: q -> (score_lettre l.letter.[0]) + (score q)

let best_first_letters wordlist =
  let rec best_letter wlist best_score words i = match wlist with
      [] ->
      if List.length words = 0 then empty_word else
      if List.length words = 1
      then List.hd words
      else best_letter wordlist 0 [] (i+1)
    | w :: q ->
      if i < (List.length w.word) then
        let score_i = score_lettre (List.nth w.word i).letter.[0] in
        if score_i > best_score then
          best_letter q score_i [w] i
        else if score_i = best_score then
          best_letter q best_score (w::words) i
        else
          best_letter q best_score words i
      else
        best_letter q best_score words i
  in best_letter wordlist 0 [] 0

let best_word wordlist =
  (* consensus : meilleur score 
     + départager avec les premières lettres *)
  let rec best_word_rec wordlist best_score words = match wordlist with
      [] -> words
    | w :: q -> 
      let score_w = score w.word in
      if score_w > best_score then
        best_word_rec q score_w [w]
      else if score_w = best_score then
        best_word_rec q best_score (w::words)
      else best_word_rec q best_score words
  in let best_words = best_word_rec wordlist 0 []
  in best_first_letters best_words

let rec add_score head score table = match table with
    [] -> [(head, score)]
  | (h,s) :: q when h = head -> (h, s+score) :: q
  | (h,s) :: q -> (h, s) :: (add_score head score q)

let record_score mot =
  (* Score des auteurs *)
  let rec record_score_rec letters = match letters with
      [] -> ()
    | l :: q ->
      let author = l.author in
      let score_l = score_lettre l.letter.[0] in
      score_table_auth := add_score author score_l !score_table_auth;
      record_score_rec q 
  in record_score_rec mot.word;
  (* Score du politicien *)
  let politician = mot.politician in
  let score_mot = score mot.word in
  score_table_pol := add_score politician score_mot !score_table_pol

let rec print_score_table table = match table with
    [] -> print_newline()
  | (h,s) :: q ->
    Printf.printf ("%d : %s \n") s h;
    print_score_table q

let sort_fun x y =
  if (snd x) < (snd y) then 1
  else if (snd x) > (snd y) then -1
  else 0

let print_score () =
  score_table_auth := List.sort sort_fun !score_table_auth;
  score_table_pol := List.sort sort_fun !score_table_pol;
  print_string "======== SCORES ========\n";
  print_newline();
  print_string "--- Auteurs ---\n";
  print_score_table !score_table_auth;
  print_newline();
  print_string "--- Politiciens ---\n";
  print_score_table !score_table_pol
