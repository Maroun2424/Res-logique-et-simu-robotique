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
      | Either _ -> failwith "Porgramme Non déterministe" (* TODO: se renseigner entre failwith ou raise *)
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

let run (prog : program) (p : point) : point list =
  failwith "À compléter"

let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  failwith "À compléter"
