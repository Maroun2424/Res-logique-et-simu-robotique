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
  

let rec unfold_repeat (prog : program) : program =
  (* RQ : je n'ai pas utilisé is_deterministic car elle ajoute un parcours complet du program *)
  List.flatten (List.map (fun instr -> match instr with (* On applatit la liste de listes d'instructions et on applique le pattern matching aux éléments *)
    | Move _ -> [instr]  (* On garde l'instruction Move dans le nouveau prog *)
    | Repeat (n , sousProg) ->
        let rec repeat extractedList nb =  (* On ajoute les instructions dépliées du sousProg à extractedList en répétant nb fois*)
            if nb = 0  then extractedList
            else repeat (extractedList @ unfold_repeat sousProg) (nb - 1)
        in repeat [] n
    | Either (prog1, prog2) ->
        let unfold1 = unfold_repeat prog1 in
        let unfold2 = unfold_repeat prog2 in
        unfold1 @ unfold2  (* On combine toutes les possibilités des deux branches *)
  ) prog)

let rec run_det (prog : program) (p : point) : point list =
  (* On génère la liste de toutes les positions visitées par le robot pendant l'exécution *)
  (* On peut utiliser unfold_repeat mais celà applatit le programme ce qui baisse l'efficacité dans le cas de long program *)
  match prog with
  | [] -> [p]  (* Cas de base : On retourne le point de départ comme seule position visitée *)
  | Move t :: reste ->  
      let nouveau_point = transform t p in (*  On applique la transformation `t` au point courant `p` pour obtenir une nouvelle position *)
      p :: run_det reste nouveau_point  (* On ajoute le `nouveau_point` à la liste des positions visitées *)
  | Repeat (n, sousProg) :: reste ->
    (*  On déplie le Repeat avec la fonction auxiliaire repeat qui crée une liste contenant n copies du sousProg
        prog_deplie est exécuté en continuant avec le reste du programme `reste` *)
      let rec repeat extractedList nb =
        if nb = 0 then extractedList   (*  On retourne toutes les répétitions de `sousProg` *)
        else repeat (extractedList @ sousProg) (nb - 1)
      in
      let prog_deplie = repeat [] n in (* programme déplié sans Repeat *)
      run_det (prog_deplie @ reste) p  (* Exécuter le programme déplié puis le reste *)
  | Either _ :: _ -> failwith "Programme Non déterministe"
  
let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  let positions_visitees = run_det prog p in (* On génère toutes les positions visitées par le robot *)
  match List.rev positions_visitees with (* On inverse la liste pour obtenir le dernier élément, qui est la position finale en tête de liste ; O(n)*)
    | [] -> false
    | derniere_pos :: _ -> in_rectangle target derniere_pos (* Vérifier si la dernière position est à l'intérieure du rectangle cible *)


(*TODO : Trouver un moyen d'enlver le code redondant*)

(* simule l'exécution d'un programme quelconque à partir d'un point de départ donné *)
let rec run (prog : program) (p : point) : point list =
  match prog with
    | [] -> [p]  (* Cas de base : On retourne le point de départ comme seule position visitée *)
    | Move t :: reste ->  
        let nouveau_point = transform t p in (*  On applique la transformation `t` au point courant `p` pour obtenir une nouvelle position *)
        p :: run reste nouveau_point  (* On ajoute le `nouveau_point` à la liste des positions visitées *)
    | Repeat (n, sousProg) :: reste ->
        (* On déplie la boucle Repeat pour exécuter le sous-programme `n` fois *)
        let rec repeat extractedList nb =
          if nb = 0 then extractedList   (*  On retourne toutes les répétitions de `sousProg` *)
          else repeat (extractedList @ sousProg) (nb - 1)
        in
        let prog_deplie = repeat [] n in (* programme déplié sans Repeat *)
        run (prog_deplie @ reste) p      (* Exécuter le programme déplié puis le reste *)
    | Either (prog1, prog2) :: reste ->
        (* On choisit aléatoirement l'une des branches avec Random.bool *)
        let choix= if  Random.bool ()  then prog1 else prog2  in
        run (choix @ reste) p  (* Exécuter le programme choisi puis le reste *)

(*TODO : Trouver un moyen d'enlver le code redondant*)

let rec all_choices (prog : program) : program list =
  match prog with
    | [] -> [[]]  (* Cas de base : un programme vide a une seule version déterministe, lui-même *)
    | Move t :: reste ->
      (* On garde  l'instruction Move et continuer sur le reste du programme *)
      List.map (fun choice -> Move t :: choice) (all_choices reste)
    | Repeat (nb, sousProg) :: reste ->
      (*On élimine le Repeat en générant toutes les combinaisons possibles pour `sousProg`  répété nb fois *)
        let sous_choices = all_choices sousProg in
        let repeated_choices =
        let rec repeat_program p nb =
            if nb = 0 then [[]]
            else
              let sub_repeats = repeat_program p (nb - 1) in
              List.flatten (List.map (fun choice -> List.map (fun sub -> choice @ sub) p) sub_repeats)
              in
                repeat_program sous_choices nb
        in
        List.flatten (List.map (fun repeated -> List.map (fun rest -> repeated @ rest) (all_choices reste)) repeated_choices)
    | Either (prog1, prog2) :: reste ->
      (* On décompose l'instruction Either en deux branches  *)
        let choices1 = all_choices prog1 in
        let choices2 = all_choices prog2 in
        let reste_choices = all_choices reste in
        (* et puis on combine chaque choix possible de Either avec le reste du programme *)
        List.flatten (
          List.map (fun choice1 -> List.map (fun rest -> choice1 @ rest) reste_choices) choices1 @
          List.map (fun choice2 -> List.map (fun rest -> choice2 @ rest) reste_choices) choices2
        )

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
