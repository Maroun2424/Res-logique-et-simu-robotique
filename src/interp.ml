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

let run_det (prog : program) (p : point) : point list =
  failwith "À compléter"

let target_reached_det (prog : program) (p : point) (target : rectangle) : bool =
  failwith "À compléter"
  
let run (prog : program) (p : point) : point list =
  failwith "À compléter"

let all_choices (prog : program) : program list =
  failwith "À compléter"

let target_reached (prog : program) (p : point) (r : rectangle) : bool =
  failwith "À compléter"
