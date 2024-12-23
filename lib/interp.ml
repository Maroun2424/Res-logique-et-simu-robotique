open Geo

(* Code de la Section 4 du projet. *)

type instruction =
  Move of transformation
| Repeat of int * program
| Either of program * program
and program = instruction list

let rec is_deterministic (prog : program) : bool =
  (* on utilise la fonction List.for_all pour vérifier si tous les éléments de prog satisfont le prédicat d'un
     programme déterministe ; pas d'instruction Either *)
  List.for_all (fun instr -> match instr with
      | Move _ -> true
      | Repeat (_ , sousProg) -> is_deterministic sousProg
      | Either _ -> false ) prog
  

(* Fonction auxiliaire pour déplier les répétitions *)
let rec unfold_repeat_aux (prog : program ) ( acc : program ) : program =
  match prog with
  | [] -> List.rev acc (* Cas de base : on ret l'accumulateur inversé *)
  | Move _ as instr :: rest ->
      unfold_repeat_aux rest (instr :: acc) (* On ajoute l'instruction à l'accumulateur*)
  | Repeat (n, sousProg) :: rest ->
      let rec repeat n acc =
        if n = 0 then acc (* Cas de base : on retourne l'accumulateur *)
        else repeat (n - 1) (List.rev_append (unfold_repeat_aux sousProg []) acc) (* On répète le sous-programme n fois*)
      in
      unfold_repeat_aux rest (repeat n acc)
  | Either (prog1, prog2) :: rest ->
      let unfolded1 = unfold_repeat_aux prog1 [] in (* première branche *)
      let unfolded2 = unfold_repeat_aux prog2 [] in (* deuxième branche *)
      unfold_repeat_aux rest (List.rev_append unfolded1 (List.rev_append unfolded2 acc))

      (*Doc : rev_append l1 l2 reverses l1 and concatenates it with l2. Equivalent to (List.rev l1) @ l2.*)

let unfold_repeat (prog : program) : program =
    unfold_repeat_aux prog [] (* récursion terminale *)

(* Fonction auxiliaire pour déplier un Repeat *)
let rec expand_repeat (n : int) (sousProg : program) (acc : program) : program =
  if n = 0 then acc (* Cas de base : on retourne l'accumulateur *)
  else expand_repeat (n - 1) sousProg (acc @ sousProg) (* On répète le sous-programme n fois *)

let rec run_det (prog : program) (p : point) : point list =
  (* On génère la liste de toutes les positions visitées par le robot pendant l'exécution *)
  (* On peut utiliser unfold_repeat mais celà applatit le programme ce qui baisse l'efficacité dans le cas de long program *)
  match prog with
  | [] -> [p]  (* Cas de base : On retourne le point de départ comme seule position visitée *)
  | Move t :: reste ->
      let nouveau_point = transform t p in (*  On applique la transformation `t` au point courant `p` pour obtenir une nouvelle position *)
      p :: run_det reste nouveau_point  (* On ajoute le `nouveau_point` à la liste des positions visitées *)
  | Repeat (n, sousProg) :: reste ->
    (*  On déplie le Repeat avec la fonction auxiliaire expand_repeat qui crée une liste contenant n copies du sousProg
        prog_deplie est exécuté en continuant avec le reste du programme `reste` *)
        let prog_deplie = expand_repeat n sousProg [] in
        run_det (prog_deplie @ reste) p
  | Either _ :: _ -> failwith "Programme Non déterministe"
  
let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  let positions_visitees = run_det prog p in (* On génère toutes les positions visitées par le robot *)
  match List.rev positions_visitees with (* On inverse la liste pour obtenir le dernier élément, qui est la position finale en tête de liste ; O(n)*)
    | [] -> false
    | derniere_pos :: _ -> in_rectangle target derniere_pos (* Vérifier si la dernière position est à l'intérieure du rectangle cible *)

(* Fonction principale pour exécuter un programme quelconque *)
let rec run (prog : program) (p : point) : point list =
  match prog with (* code similaire à run_det*)
  | [] -> [p]
  | Move t :: reste ->
      let nouveau_point = transform t p in
      p :: run reste nouveau_point
  | Repeat (n, sousProg) :: reste ->
      let prog_deplie = expand_repeat n sousProg [] in
      run (prog_deplie @ reste) p
  | Either (prog1, prog2) :: reste ->
      let choix= if Random.bool () then prog1 else prog2 in (* cas Either : on choisit aléatoirement une branche *)
      run (choix @ reste) p

(* Fonction auxilaiire :
    si p1 =[[A];[B]] et p2 =[[C];[D]] alors combine p1 p2 = [[A;C];[A;D];[B;C];[B;D]]
*)
let combine p1 p2 =
  List.flatten (List.map (fun x -> List.map (fun y -> x @ y) p2) p1)


let rec all_choices (prog : program) : program list =
  match prog with
  | [] -> [[]]
  | Move t :: reste ->
      List.map (fun choice -> Move t :: choice) (all_choices reste)
  | Repeat (n, sousProg) :: reste ->
      let sous_choices = all_choices sousProg in
      (* Définition de la fonction interne de répétition *)
      let rec repeat_program p nb =
        if nb = 0 then [[]]
        else
          let sub_repeats = repeat_program p (nb - 1) in
          (* Pour chaque liste dans sub_repeats, on concatène avec tous les choix dans p *)
          combine sub_repeats p
      in
      let repeated_choices = repeat_program sous_choices n in
      let reste_choices = all_choices reste in
      combine repeated_choices reste_choices
  | Either (prog1, prog2) :: reste ->
      (* On décompose l'instruction Either en deux branches  *)
      let choices1 = all_choices prog1 in
      let choices2 = all_choices prog2 in
      let reste_choices = all_choices reste in
      (* et puis on combine chaque choix possible de Either avec le reste du programme *)
      combine choices1 reste_choices @ combine choices2 reste_choices

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  (* Étape 1 : on génère toutes les exécutions possibles *)
    let executions = all_choices prog in
    (* Étape 2 : On vérifie que toutes les exécutions terminent dans  le rectangle cible*)
    List.for_all (fun det_prog ->
    (*La position finale ( queue of the list ) pour chaque programme déterministe *)
      match List.rev (run_det det_prog p) with
        | [] -> false (* Aucune position n'est retournée *)
        | final_position :: _ -> in_rectangle r final_position
    ) executions
