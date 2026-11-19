# Typeur et évaluateur pour un λ-calcul enrichi

Projet TAS 1, partie Typage (M2 STL, Sorbonne Université).

Auteur : PENG Kairui - 21219046

[中文版](README.zh.md)

Le projet est écrit en OCaml, dans un seul fichier `minitypeur.ml`. Il contient :

- un **typeur** : inférence de types par génération d'équations puis unification ;
- un **évaluateur** : réduction pas à pas en Left-to-Right Call-by-Value.

Le point de départ est le fichier `minitypeur.ml` donné sur Moodle (premier commit, `b8c5eb7`).

## Lancer le projet

```
ocaml minitypeur.ml
```

Testé avec OCaml 4.14.0, sans bibliothèque externe. Le programme affiche :

1. le typage de chaque exemple : `***TYPABLE*** avec le type ...` ou `***PAS TYPABLE*** : raison` ;
2. l'évaluation de chaque exemple, avec une ligne `-->` par étape (et la mémoire si elle n'est pas vide), puis le résultat :
   - `***VALEUR***` : le terme s'est réduit en une valeur,
   - `***BLOQUE***` : le terme ne se réduit plus mais n'est pas une valeur,
   - `***TIMEOUT***` : trop d'étapes,
   - `***EXCEPTION***` : exception non rattrapée.

Pour lancer un programme écrit dans un fichier texte :

```
ocaml minitypeur.ml exemples/somme.txt
```

Le programme est lu par le parser, puis typé et évalué. Le dossier `exemples/` contient quelques programmes : `somme.txt`, `map.txt`, `faible.txt` et `points.txt`.

Dans `minitypeur.ml`, les exemples sont écrits avec les constructeurs (`let ex_... : pterm = ...`), et on les ajoute dans `main` pour les afficher. À la fin, `main` lit aussi quelques programmes écrits en texte, et vérifie que le parser relit exactement tout ce que `print_term` affiche.

## Tests

```
ocaml tests/tests.ml
```

À lancer depuis la racine du projet. Le fichier charge `minitypeur.ml` (sans lancer `main`) et vérifie 139 cas : si le typage accepte ou refuse le terme, le résultat de l'évaluation, la lecture par le parser (priorités et erreurs de syntaxe), la relecture de tous les exemples, et les programmes du dossier `exemples/`. Il affiche le nombre de tests réussis pour chaque partie, et le détail seulement quand un test échoue.

## Organisation du fichier

| Partie      | Contenu                                                                                                  |
| ----------- | -------------------------------------------------------------------------------------------------------- |
| syntaxe     | types `pterm` (termes) et `ptype` (types), pretty printers `print_term` et `print_type`          |
| types       | occur check, substitution, table des classes, unification (`unif_etape`, `resout`), généralisation |
| typage      | génération d'équations `genere_equa`, inférence `inference`                                      |
| évaluation | `alphaconv`, `substitution`, `ltr_cbv_step`, `ltr_cbv_norm`, `evaluation`                      |
| parser | `lexer`, `lit_expr` (une fonction par niveau de priorité), `lit_type`, `lit_programme`, `execute` |
| exemples    | les termes `ex_...` et la fonction `main`                                                            |

## Ce qui est implémenté

### Partie 2 : λ-calcul simplement typé

- alpha-conversion vers la convention de Barendregt (nouveaux noms `x_1`, `y_2`, ...) ;
- substitution, une étape de réduction LtR-CbV, affichage des réduits avec un timeout (nombre maximal d'étapes) ;
- unification par étapes (`unif_etape`) et résolution avec timeout (`resout`).

### Partie 3 : entiers, listes, let

- `+`, `-`, listes natives (`[]`, `::`, `hd`, `tl`), `if zero`, `if empty`, `fix`, `let` ;
- types `Nat`, `[T]` et `∀X.T` ; let-polymorphisme.

### Partie 4 : traits impératifs

- `()`, `ref e`, `!e`, `e1 := e2`, types `unit` et `Ref T` ; la mémoire (`rho1 -> ...`) est affichée à chaque étape ;
- polymorphisme faible : l'exemple du sujet `let l = ref [] in let _ = l := [(fun x -> x)] in (hd !l) + 2` est refusé.

### Partie 5 : extensions

- **enregistrements** `{x = 1; y = 2}` et `e.x`, avec polymorphisme de rangée : `fun p -> p.x` a le type `{x : T | ρ} -> T` ;
- **option** : `None`, `Some e`, `match e with None -> e1 | Some x -> e2` ;
- **exceptions** : `raise e` et `try e1 with x -> e2` ;
- **objets** : classes avec attributs typés et méthodes, `new`, `self`, accès `o#a`, appel `o#m(arg)`, héritage, redéfinition avec liaison dynamique, sous-typage, surcharge, fonctions annotées `fun (x : T) -> e` et upcast `(e :> C)`.

### Bonus : lexer et parser

Le parser est écrit à la main (descente récursive, sans ocamllex ni menhir). Il lit la syntaxe affichée par `print_term`, avec les priorités habituelles d'OCaml, donc on n'a pas besoin de tout parenthéser.

| Construction | Syntaxe |
|---|---|
| fonctions | `fun x y -> e`, `fun (x : T) -> e`, `f a b` |
| let | `let x = e1 in e2`, `let f x y = e1 in e2`, `let rec f n = e1 in e2` |
| entiers et listes | `1 + 2 - 3`, `[]`, `x :: l`, `[1; 2; 3]`, `hd l`, `tl l` |
| branchements, point fixe | `if zero e then e1 else e2`, `if empty e then e1 else e2`, `fix e` |
| références | `()`, `ref e`, `!e`, `e1 := e2` |
| enregistrements | `{x = 1; y = 2}`, `e.x` |
| option | `None`, `Some e`, `match e with None -> e1 \| Some x -> e2` |
| exceptions | `raise e`, `try e1 with x -> e2` |
| objets | `class C extends D = {a : T; m (x : T) : T = e} in e2`, `new C {a = e}`, `o#a`, `o#m(e)`, `o#m()`, `(e :> C)` |
| types | `Nat`, `unit`, `T -> T`, `[T]`, `Ref T`, `Option T`, `{a : T}`, `C` |
| commentaires | `(* ... *)` |

Priorités, de la plus faible à la plus forte : `fun`, `let`, `if`, `match`, `try` et `class` (qui s'étendent le plus loin possible), puis `:=`, `::` (à droite), `+` et `-` (à gauche), l'application, les opérateurs préfixes (`hd`, `tl`, `ref`, `raise`, `fix`, `Some`, `!`), et enfin `.x` et `#m`.

`let rec f x = e1 in e2` est traduit en `let f = fix (fun f -> fun x -> e1) in e2`.

## Choix d'implémentation

- **Opérateurs** : comme `Add` dans le fichier de départ, ce sont des constructeurs de termes, et on génère directement leurs équations. C'est équivalent aux types `N → N → N` ou `∀X.[X] → X` du sujet.
- **Unification** : l'unification du fichier de départ (avec zipper) a été remplacée par une version par étapes qui renvoie la liste des variables résolues. On en a besoin pour le let.
- **let** : on type `e1` seul, on généralise par rapport à l'environnement (après lui avoir appliqué la solution), et on garde aussi les équations de `e1` dans le système global. Sinon une contrainte sur une variable de λ est perdue, et `(fun y -> let x = y + 1 in y) []` serait accepté.
- **∀** : dans l'unification, les équations avec un ∀ sont traitées en premier .
- **Polymorphisme faible** : on ne généralise que les termes non expansifs, et une application est toujours expansive (restriction aux valeurs, comme en OCaml). Le cours dit qu'une application dont les sous-termes sont non expansifs est non expansive, mais ce n'est pas correct : avec `let mkref = fun x -> ref x in let l = mkref [] in ...` on retrouve le problème du sujet (exemple `ex_mkref`).
- **Rangées** : un accès `e.l` génère l'équation `T = {l : T' | ρ}`. Les variables de rangée sont généralisées comme les variables de type.
- **Exceptions** : une exception porte un entier. Quand on évalue `raise v`, on lève une exception OCaml `Levee v`, rattrapée par le `try` le plus proche.
- **Sous-typage** : les équations sont orientées (type attendu, type effectif). Deux classes s'unifient si l'effectif est une sous-classe de l'attendu, la flèche est contravariante à gauche, et `Ref` est invariant (sinon on pourrait mettre un `Point` dans une `Ref PointC`).
- **Surcharge** : on choisit la méthode dont la classe du paramètre est la plus proche de celle de l'argument (distance d'héritage, comme en Java). La surcharge n'est possible que sur des paramètres de type classe, avec le même type de retour.

## Limites

- `hd []` et `tl []` sont typables mais bloquent : le typage ne le détecte pas (on peut utiliser une option ou une exception).
- Pour lire un attribut ou appeler une méthode, le typeur doit connaître la classe de l'objet : un paramètre utilisé comme objet doit être annoté (`fun (p : Point) -> p#x`).
- Le sous-typage est correct mais incomplet : par exemple la liste `[new PointC ...; new Point ...]` est refusée, il faut écrire `(new PointC ... :> Point)`.
- La surcharge est choisie avec le type statique au typage, mais avec la classe réelle de l'argument à l'exécution. Après un upcast, on peut donc appeler une autre méthode que Java. Les deux ont le même type de retour, donc le typage reste correct.
- Les timeouts sont des nombres d'étapes : 100 (ou 10 pour Ω) pour l'évaluation, 10000 pour l'unification.
- Parser : pas d'entiers négatifs (écrire `0 - 1`), les commentaires ne s'imbriquent pas, et un `fun` annoté ne prend qu'un paramètre.

## Quelques exemples

| Exemple          | Ce qu'il montre                                      | Résultat                                 |
| ---------------- | ---------------------------------------------------- | ----------------------------------------- |
| `ex_omega`     | Ω                                                   | pas typable, `TIMEOUT` à l'évaluation |
| `ex_poly`      | `let i = fun x -> x in i i 2`                     | `Nat`, vaut 2                           |
| `ex_faible`    | l'exemple du sujet avec `ref []`                   | pas typable                               |
| `ex_cours_ref` | l'exemple du cours 04                                | pas typable, bloque sur `() + 1`        |
| `ex_mkref`     | une application est expansive                        | pas typable                               |
| `ex_rangee`    | `getx` sur deux enregistrements différents        | `Nat`, vaut 11                          |
| `ex_exn_f`     | `raise` dans une fonction appelée dans un `try` | `Nat`, vaut 5                           |
| `ex_heritage`  | sous-typage et liaison dynamique                     | `Nat`, vaut 163                         |
| `ex_surcharge` | surcharge `dist(P)` / `dist(PE)`                | `{avec_p = 9; avec_pe = 18}`            |
| `ex_let_contrainte2` | le let garde la contrainte `y : Nat` | pas typable, bloque sur `[] + 1` |
| `ex_hd_vide` | `hd []` | typable, mais bloque à l'évaluation |
| `ex_ref_invariant` | `Ref` invariant : un `Point` dans une `Ref PointC` | pas typable, bloque sur `#c` |
| `ex_surcharge_upcast` | surcharge après un upcast | `Nat`, vaut 18 (9 en Java) |
