(* tests du typeur, de l'évaluateur et du parser, à lancer depuis la racine :  ocaml tests/tests.ml *)
#use "./minitypeur.ml";;

let n_ok : int ref = ref 0
let n_ko : int ref = ref 0
let ok_avant : int ref = ref 0
let ko_avant : int ref = ref 0

(* est-ce que s contient m ? *)
let contient (s : string) (m : string) : bool =
  let n : int = String.length s and k : int = String.length m in
  let rec cherche (i : int) : bool = i + k <= n && (String.sub s i k = m || cherche (i + 1)) in
  cherche 0

(* typage : on vérifie seulement accepté / refusé, le reste de attendu est pour le lecteur *)
let teste (attendu : string) (t : pterm) : unit =
  let res : string = inference t in
  if contient res "***PAS TYPABLE***" = contient attendu "PAS TYPABLE" then incr n_ok
  else (incr n_ko; print_endline ("ECHEC typage [attendu " ^ attendu ^ "] " ^ res))

(* évalue sans afficher les étapes *)
let calcule (t : pterm) : string =
  memoire := []; compteur_region := 0; classes := [];
  let rec boucle (t : pterm) (n : int) : string =
    if n = 0 then "TIMEOUT"
    else match ltr_cbv_step t with
        None -> if est_valeur t then print_term t else "BLOQUE " ^ (print_term t)
      | Some t2 -> boucle t2 (n - 1) in
  try boucle t 1000 with Levee v -> "EXCEPTION " ^ (print_term v)

(* évaluation : résultat exact, ou seulement BLOQUE / TIMEOUT *)
let verifie_eval (attendu : string) (t : pterm) : unit =
  let res : string = calcule t in
  if res = attendu || (List.mem attendu ["BLOQUE"; "TIMEOUT"] && contient res attendu) then incr n_ok
  else (incr n_ko; print_endline ("ECHEC eval : " ^ (print_term t) ^ "\n   obtenu  : " ^ res ^ "\n   attendu : " ^ attendu))

(* parser : le texte doit donner le terme attendu *)
let pareil (s : string) (attendu : pterm) : unit =
  let res : string = (try print_term (lit_programme s) with Erreur_syntaxe m -> "ERREUR " ^ m) in
  if res = print_term attendu then incr n_ok
  else (incr n_ko; print_endline ("ECHEC parser : " ^ s ^ "\n   lu      : " ^ res ^ "\n   attendu : " ^ (print_term attendu)))

(* parser : le texte doit donner une erreur de syntaxe *)
let erreur (s : string) : unit =
  try (let t : pterm = lit_programme s in incr n_ko; print_endline ("ECHEC parser (pas d'erreur) : " ^ s ^ " lu " ^ (print_term t)))
  with Erreur_syntaxe _ -> incr n_ok

let bilan (nom : string) : unit =
  Printf.printf "%-34s %3d ok, %d echecs\n" nom (!n_ok - !ok_avant) (!n_ko - !ko_avant);
  ok_avant := !n_ok; ko_avant := !n_ko;;

(* let et polymorphisme *)
teste "Nat -> Nat" (Abs ("y", Let ("x", Add (Var "y", N 1), Var "y")));;
teste "PAS TYPABLE" (App (Abs ("y", Let ("x", Add (Var "y", N 1), Var "y")), Nil));;
teste "T -> T" (Abs ("y", Let ("x", Var "y", Var "x")));;
teste "PAS TYPABLE" (Abs ("y", Let ("x", Var "y", Add (App (Var "x", N 1), Var "x"))));;
teste "[Nat]" (Let ("f", Abs ("x", Var "x"), Cons (App (Var "f", N 1), App (Var "f", Nil))));;
teste "Nat" (Let ("f", Abs ("x", Var "x"), Let ("g", Var "f", App (App (Var "g", Var "g"), N 1))));;
teste "[Nat]" (Let ("x", Nil, Cons (N 1, Var "x")));;
teste "PAS TYPABLE" (Let ("x", Nil, IfEmpty (Var "x", Cons (N 1, Var "x"), Cons (Nil, Var "x"))));;
teste "PAS TYPABLE" (Abs ("f", App (App (Var "f", Var "f"), N 1)));;
teste "T" (Hd Nil);;
teste "PAS TYPABLE (variable libre)" (Add (Var "z", N 1));;
let ex_map : pterm = Fix (Abs ("map", Abs ("f", Abs ("l", IfEmpty (Var "l", Nil,
  Cons (App (Var "f", Hd (Var "l")), App (App (Var "map", Var "f"), Tl (Var "l"))))))));;
teste "(A -> B) -> [A] -> [B]" ex_map;;
teste "[Nat]" (App (App (ex_map, Abs ("x", Add (Var "x", N 1))), Cons (N 1, Cons (N 2, Nil))));;
teste "[[Nat]]" (Let ("map", ex_map, App (App (Var "map", Abs ("l", App (App (Var "map", Abs ("x", Sub (Var "x", N 1))), Var "l"))),
  Cons (Cons (N 5, Nil), Nil))));;
verifie_eval "(2 :: (3 :: []))" (App (App (ex_map, Abs ("x", Add (Var "x", N 1))), Cons (N 1, Cons (N 2, Nil))));;
verifie_eval "BLOQUE" (Hd Nil);;
verifie_eval "12" (Let ("x", Add (N 1, N 1), Let ("x", Add (Var "x", N 10), Var "x")));;
verifie_eval "TIMEOUT" (Fix (Abs ("f", Add (Var "f", N 1))));;
bilan "let et polymorphisme";;

(* références et polymorphisme faible *)
let id : pterm = Abs ("x", Var "x");;
teste "Nat" (Let ("r", Ref id, Let ("_", Assign (Var "r", Abs ("x", Add (Var "x", N 1))), App (Deref (Var "r"), N 3))));;
teste "PAS TYPABLE" (Let ("r", Ref id, Let ("_", Assign (Var "r", Abs ("x", Add (Var "x", N 1))), App (Deref (Var "r"), Nil))));;
teste "T -> Ref T" (Abs ("x", Ref (Var "x")));;
teste "Nat" (Deref (Ref (N 5)));;
teste "unit" (Assign (Ref (N 1), N 2));;
teste "PAS TYPABLE" (App (Abs ("r", Assign (Var "r", N 1)), Ref Nil));;
(* chaque appel de f crée une ref indépendante *)
teste "Nat" (Let ("f", Abs ("u", Ref Nil), Let ("a", App (Var "f", Unit), Let ("b", App (Var "f", Unit),
  Let ("_", Assign (Var "a", Cons (N 1, Nil)), Let ("_", Assign (Var "b", Cons (Nil, Nil)), N 0))))));;
(* f capture l (faible) donc f n'est pas généralisée *)
teste "Nat" (Let ("l", Ref Nil, Let ("f", Abs ("x", Assign (Var "l", Cons (Var "x", Nil))),
  Let ("_", App (Var "f", N 1), Hd (Deref (Var "l"))))));;
teste "PAS TYPABLE" (Let ("l", Ref Nil, Let ("f", Abs ("x", Assign (Var "l", Cons (Var "x", Nil))),
  Let ("_", App (Var "f", N 1), Let ("_", App (Var "f", Nil), N 0)))));;
(* une application n'est pas généralisée *)
teste "PAS TYPABLE" (Let ("g", App (id, id), Let ("_", App (Var "g", N 1), App (Var "g", Nil))));;
teste "Nat" (Let ("g", id, Let ("_", App (Var "g", N 1), Let ("_", App (Var "g", Nil), N 0))));;
teste "PAS TYPABLE (region)" (Region 1);;
verifie_eval "4" (Let ("r", Ref id, Let ("_", Assign (Var "r", Abs ("x", Add (Var "x", N 1))), App (Deref (Var "r"), N 3))));;
verifie_eval "2" (Let ("a", Ref (N 1), Let ("b", Ref (N 2), Let ("_", Assign (Var "a", Deref (Var "b")), Deref (Var "a")))));;
verifie_eval "BLOQUE" (Deref (N 3));;
bilan "references, polymorphisme faible";;

(* enregistrements et option *)
let xy : pterm = Abs ("p", Add (Champ (Var "p", "x"), Champ (Var "p", "y")));;
teste "{x : Nat; y : Nat | r} -> Nat" xy;;
teste "PAS TYPABLE (y absent)" (App (xy, Record [("x", N 1)]));;
teste "Nat (champ en plus ok)" (App (xy, Record [("x", N 1); ("y", N 2); ("z", N 3)]));;
teste "PAS TYPABLE (double)" (Record [("x", N 1); ("x", N 2)]);;
teste "{a : Nat; b : Nat}" (IfZero (N 0, Record [("a", N 1); ("b", N 2)], Record [("b", N 3); ("a", N 4)]));;
teste "PAS TYPABLE" (IfZero (N 0, Record [("a", N 1)], Record [("b", N 2)]));;
teste "{a; b | r} -> {a; b | r} -> {a; b | r}" (Abs ("p", Abs ("q", Let ("_", Champ (Var "p", "a"),
  Let ("_", Champ (Var "q", "b"), IfZero (N 0, Var "p", Var "q"))))));;
teste "[T] (record de fonction polymorphe)" (Let ("r", Record [("id", Abs ("x", Var "x"))],
  Let ("a", App (Champ (Var "r", "id"), N 1), App (Champ (Var "r", "id"), Nil))));;
teste "{pos : {x : T | r1} | r2} -> T" (Abs ("p", Champ (Champ (Var "p", "pos"), "x")));;
teste "PAS TYPABLE (rangee recursive)" (Abs ("p", App (Champ (Var "p", "x"), Var "p")));;
teste "PAS TYPABLE (pas de x)" (Let ("getx", Abs ("p", Champ (Var "p", "x")), App (Var "getx", Record [("y", N 1)])));;
teste "[T]" (Let ("getx", Abs ("p", Champ (Var "p", "x")), Let ("a", App (Var "getx", Record [("x", N 1)]),
  App (Var "getx", Record [("x", Nil); ("w", N 2)]))));;
teste "(Option Nat) -> Nat" (Abs ("o", MatchOpt (Var "o", N 0, "x", Add (Var "x", N 1))));;
teste "Nat" (Let ("r", Ref OptNone, Let ("_", Assign (Var "r", OptSome (N 1)), MatchOpt (Deref (Var "r"), N 0, "v", Var "v"))));;
teste "PAS TYPABLE (faible)" (Let ("r", Ref OptNone, Let ("_", Assign (Var "r", OptSome (Abs ("x", Var "x"))),
  MatchOpt (Deref (Var "r"), N 0, "v", Add (Var "v", N 1)))));;
teste "Nat" (Let ("c", Record [("n", Ref (N 0))], Let ("_", Assign (Champ (Var "c", "n"), N 5), Deref (Champ (Var "c", "n")))));;
verifie_eval "3" (App (xy, Record [("x", N 1); ("y", N 2); ("z", N 3)]));;
verifie_eval "{a = (); b = 1}" (Let ("c", Ref (N 0), Record [("a", Assign (Var "c", N 1)); ("b", Deref (Var "c"))]));;
verifie_eval "5" (Let ("c", Record [("n", Ref (N 0))], Let ("_", Assign (Champ (Var "c", "n"), N 5), Deref (Champ (Var "c", "n")))));;
bilan "enregistrements et option";;

(* exceptions *)
teste "PAS TYPABLE (branches)" (Try (N 1, "e", Nil));;
teste "Nat -> Nat" (Abs ("x", Try (Var "x", "e", Var "e")));;
teste "(Nat -> Nat) -> Nat" (Abs ("f", Try (App (Var "f", N 0), "e", Var "e")));;
teste "T (raise en position de fonction)" (App (Raise (N 1), N 2));;
teste "Nat" (Let ("x", Raise (N 1), Add (Var "x", N 1)));;
let compte : pterm = Fix (Abs ("f", Abs ("n", IfZero (Var "n", Raise (N 3), App (Var "f", Sub (Var "n", N 1))))));;
teste "Nat" (Try (App (compte, N 5), "e", Add (Var "e", N 100)));;
teste "Nat" (Try (Champ (Record [("a", N 1); ("b", Raise (N 4))], "a"), "e", Var "e"));;
teste "Nat" (Try (Hd (Cons (N 1, Raise (N 2))), "e", Add (Var "e", N 1)));;
teste "Nat" (Let ("r", Ref (N 0), Let ("_", Try (Assign (Var "r", Raise (N 9)), "e", Assign (Var "r", Var "e")), Deref (Var "r"))));;
teste "PAS TYPABLE (e est un entier)" (Try (N 1, "e", Hd (Var "e")));;
verifie_eval "EXCEPTION 1" (App (Raise (N 1), N 2));;
verifie_eval "EXCEPTION 1" (Let ("x", Raise (N 1), Add (Var "x", N 1)));;
verifie_eval "103" (Try (App (compte, N 5), "e", Add (Var "e", N 100)));;
verifie_eval "4" (Try (Champ (Record [("a", N 1); ("b", Raise (N 4))], "a"), "e", Var "e"));;
verifie_eval "3" (Try (Hd (Cons (N 1, Raise (N 2))), "e", Add (Var "e", N 1)));;
verifie_eval "9" (Let ("r", Ref (N 0), Let ("_", Try (Assign (Var "r", Raise (N 9)), "e", Assign (Var "r", Var "e")), Deref (Var "r"))));;
verifie_eval "1" (Try (N 1, "e", N 2));;
bilan "exceptions";;

(* objets *)
let pts (corps : pterm) : pterm = classe_point (classe_pointc corps);;
let pt : pterm = New ("Point", [("x", N 1); ("y", N 2)]);;
let ptc : pterm = New ("PointC", [("x", N 10); ("y", N 20); ("c", N 30)]);;
teste "PAS TYPABLE (attribut manquant)" (pts (New ("Point", [("x", N 1)])));;
teste "PAS TYPABLE (classe inconnue)" (New ("Truc", []));;
teste "PAS TYPABLE (pas un objet)" (Appel (N 3, "m", N 1));;
teste "PAS TYPABLE (methode inconnue)" (pts (Appel (pt, "vole", Unit)));;
teste "PAS TYPABLE (parametre non annote)" (pts (App (Abs ("p", Attribut (Var "p", "x")), pt)));;
let compteur : pterm = Classe ("Compteur", None, [("n", Nat)],
  [("jusqua", "k", Nat, Nat, IfZero (Var "k", Attribut (Var "self", "n"), Add (Appel (Var "self", "jusqua", Sub (Var "k", N 1)), N 1)))],
  Appel (New ("Compteur", [("n", N 5)]), "jusqua", N 3));;
teste "Nat (recursion par self)" compteur;;
(* Ref invariant : sinon on pourrait mettre un Point dans une ref de PointC *)
teste "PAS TYPABLE (Ref invariant)" (pts (Let ("r", Ref ptc, Let ("f", AbsT ("q", TRef (TClasse "Point"), Assign (Var "q", pt)),
  Let ("_", App (Var "f", Var "r"), Attribut (Deref (Var "r"), "c"))))));;
teste "Nat (un PointC dans une ref de Point)" (pts (Let ("r", Ref pt, Let ("_", Assign (Var "r", ptc), Attribut (Deref (Var "r"), "x")))));;
(* flèche contravariante à gauche *)
teste "PAS TYPABLE (contravariance)" (pts (Let ("f", AbsT ("g", Arr (TClasse "Point", Nat), App (Var "g", pt)),
  App (Var "f", AbsT ("p", TClasse "PointC", Attribut (Var "p", "c"))))));;
teste "Nat (contravariance ok)" (pts (Let ("f", AbsT ("g", Arr (TClasse "PointC", Nat), App (Var "g", ptc)),
  App (Var "f", AbsT ("p", TClasse "Point", Attribut (Var "p", "x"))))));;
(* redéfinition avec un type de retour plus précis *)
let ab : pterm = Classe ("A", None, [], [("m", "x", Nat, TClasse "A", Var "self")],
  Classe ("B", Some "A", [("k", Nat)], [("m", "x", Nat, TClasse "B", Var "self")], Attribut (Appel (New ("B", [("k", N 7)]), "m", N 1), "k")));;
teste "Nat (retour covariant)" ab;;
teste "PAS TYPABLE (coerce vers une sous-classe)" (pts (Coerce (pt, TClasse "PointC")));;
teste "PAS TYPABLE (parent inconnu)" (Classe ("C", Some "Rien", [], [], N 0));;
teste "PAS TYPABLE (attribut en double)" (pts (Classe ("D", Some "Point", [("x", Nat)], [], N 0)));;
teste "PAS TYPABLE (type statique Point)" (pts (Let ("f", AbsT ("p", TClasse "Point", Var "p"), Attribut (App (Var "f", ptc), "c"))));;
teste "Nat (let polymorphe et objet)" (pts (Let ("id", Abs ("x", Var "x"), Attribut (App (Var "id", pt), "x"))));;
teste "Nat (surcharge apres upcast)" ex_surcharge_upcast;;
verifie_eval "8" compteur;;
verifie_eval "10" (pts (Let ("r", Ref pt, Let ("_", Assign (Var "r", ptc), Attribut (Deref (Var "r"), "x")))));;
verifie_eval "7" ab;;
verifie_eval "18" ex_surcharge_upcast;;
bilan "objets";;

(* parser *)
let v (x : string) : pterm = Var x;;
pareil "fun x -> x + 1" (Abs ("x", Add (v "x", N 1)));;
pareil "f x y" (App (App (v "f", v "x"), v "y"));;
pareil "f x + g y" (Add (App (v "f", v "x"), App (v "g", v "y")));;
pareil "a - b - c" (Sub (Sub (v "a", v "b"), v "c"));;
pareil "1 + 2 :: 3 :: []" (Cons (Add (N 1, N 2), Cons (N 3, Nil)));;
pareil "[1; 2; 3]" (Cons (N 1, Cons (N 2, Cons (N 3, Nil))));;
pareil "r := !r + 1" (Assign (v "r", Add (Deref (v "r"), N 1)));;
pareil "!f ()" (App (Deref (v "f"), Unit));;
pareil "hd l + 1" (Add (Hd (v "l"), N 1));;
pareil "hd (tl l)" (Hd (Tl (v "l")));;
pareil "fix (fun f -> f) 3" (App (Fix (Abs ("f", v "f")), N 3));;
pareil "fun x y -> x" (Abs ("x", Abs ("y", v "x")));;
pareil "let f x y = x in f" (Let ("f", Abs ("x", Abs ("y", v "x")), v "f"));;
pareil "let rec f n = f n in f 1" (Let ("f", Fix (Abs ("f", Abs ("n", App (v "f", v "n")))), App (v "f", N 1)));;
pareil "match o with None -> 0 | Some x -> x + 1" (MatchOpt (v "o", N 0, "x", Add (v "x", N 1)));;
pareil "try f 0 with e -> e" (Try (App (v "f", N 0), "e", v "e"));;
pareil "raise (x + 1)" (Raise (Add (v "x", N 1)));;
pareil "Some 5" (OptSome (N 5));;
pareil "p.pos.x" (Champ (Champ (v "p", "pos"), "x"));;
pareil "o#m(1)#n()" (Appel (Appel (v "o", "m", N 1), "n", Unit));;
pareil "o#a + 1" (Add (Attribut (v "o", "a"), N 1));;
pareil "(e :> Point)" (Coerce (v "e", TClasse "Point"));;
pareil "fun (g : Point -> Nat) -> g" (AbsT ("g", Arr (TClasse "Point", Nat), v "g"));;
pareil "fun (r : Ref [Nat]) -> r" (AbsT ("r", TRef (Liste Nat), v "r"));;
pareil "fun (o : Option {a : Nat}) -> o" (AbsT ("o", TOption (TRecord ([("a", Nat)], None)), v "o"));;
pareil "let _ = r := 1 in !r" (Let ("_", Assign (v "r", N 1), Deref (v "r")));;
pareil "if zero n then 0 else n + f (n - 1)" (IfZero (v "n", N 0, Add (v "n", App (v "f", Sub (v "n", N 1)))));;
pareil "(* commentaire *) 1 (* encore *) + 2" (Add (N 1, N 2));;
pareil "{a = fun x -> x; b = 2}" (Record [("a", Abs ("x", v "x")); ("b", N 2)]);;
pareil "f new P {a = 1}" (App (v "f", New ("P", [("a", N 1)])));;
pareil "class A extends B = {} in 0" (Classe ("A", Some "B", [], [], N 0));;
pareil "x' + x_1" (Add (v "x'", v "x_1"));;
erreur "let x = in 3";;
erreur "(1 + 2";;
erreur "1 + @";;
erreur "(* pas ferme";;
erreur "1 2 )";;
erreur "fun -> 1";;
erreur "match x with Some y -> 1";;
erreur "-1";;
(* le parser relit tout ce que print_term écrit *)
let () = if contient (relecture tous_les_exemples) ": OK" then incr n_ok
  else (incr n_ko; print_endline (relecture tous_les_exemples));;
bilan "parser";;

(* programmes du dossier exemples/ *)
let fichier (nom : string) : pterm = lit_programme (lit_fichier ("exemples/" ^ nom));;
teste "Nat" (fichier "somme.txt");;
verifie_eval "10" (fichier "somme.txt");;
teste "PAS TYPABLE" (fichier "faible.txt");;
verifie_eval "BLOQUE" (fichier "faible.txt");;
teste "Nat" (fichier "points.txt");;
verifie_eval "163" (fichier "points.txt");;
teste "{entiers : [Nat]; listes : [[Nat]]}" (fichier "map.txt");;
verifie_eval "{entiers = (2 :: (3 :: (4 :: []))); listes = ((0 :: []) :: ((0 :: (5 :: [])) :: []))}" (fichier "map.txt");;
bilan "programmes de exemples/";;

let () = Printf.printf "total : %d ok, %d echecs\n" !n_ok !n_ko;;
let () = if !n_ko > 0 then exit 1;;
