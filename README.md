# Projet PF5

## Installation d'`opam`

Pour commencer, installez le gestionnaire de paquets [`opam`](https://opam.ocaml.org/) en suivant les instructions données [ici](https://opam.ocaml.org/doc/Install.html).

## Installation des paquets

Placez-vous dans le répertoire cloné.
De là, exécutez les commandes suivantes, qui créent un switch `opam` local en y installant les paquets nécessaires :

```
opam update
opam switch create . 4.14.1 -y --deps-only
```

## Compilation

Pour compiler le projet, exécutez la commande `make`.

## Toplevel

Afin de vous-même tester et déboguer, vous pouvez utiliser le toplevel `utop` qui a été installé.
Pour le lancer, exécutez la commande `make top`.

## Tests

Pour lancer tous les tests disponibles, exécutez `make test`.
Pour tester seulement les fonctions de l'exercice *i*, exécutez `make test-i`.
pour lancer les tests personnels ajoutés :
 - Module geo.ml : `make test-geo`
 - Module interp.ml : `make test-interp`
 - Module approx.ml : `make test-approx`

## Lancer l'interpreteur

Le fichier qui contient la fonction "main" du project est `bin/interp.ml` .
La commande pour compiler le projet est `dune build`,
et celle pour compiler et lancer le main est `dune exec interp` suivie des options et des arguments éventuels.

update readme.md
pour tester nos programmes, il faut suivre cette syntaxe:
dune exec interp (*pour executer le program 2 par defaut, et ca marche sans arguments*)
dune exec interp -- n -k où n est le numero du program et k sont les arguments ou inversement

par exemple pour tester le programme 2, avec l'option -size et -pc, la commande est : 
dune exec interp -- 2 -pc 255 255 0 -size 600 600

