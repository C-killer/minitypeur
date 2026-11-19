(* PENG Kairui - 21219046 *)
(* Termes *)
type pterm = Var of string | App of pterm * pterm | Abs of string * pterm | N of int | Add of pterm * pterm
  | Sub of pterm * pterm | Nil | Cons of pterm * pterm | Hd of pterm | Tl of pterm
  | IfZero of pterm * pterm * pterm | IfEmpty of pterm * pterm * pterm
  | Fix of pterm | Let of string * pterm * pterm
  | Unit | Region of int | Ref of pterm | Deref of pterm | Assign of pterm * pterm
  | Record of (string * pterm) list | Champ of pterm * string
  | OptNone | OptSome of pterm | MatchOpt of pterm * pterm * string * pterm
  | Raise of pterm | Try of pterm * string * pterm
  | Classe of string * string option * (string * ptype) list * methode list * pterm
  | New of string * (string * pterm) list | Objet of string * (string * pterm) list
  | Attribut of pterm * string | Appel of pterm * string * pterm
  | AbsT of string * ptype * pterm | Coerce of pterm * ptype 
(* Types *) 
and ptype = Var of string | Arr of ptype * ptype | Nat | Liste of ptype | Forall of string list * ptype | TUnit | TRef of ptype | TOption of ptype | TRecord of (string * ptype) list * string option | TClasse of string
(* méthode : nom, paramètre, type du paramètre, type de retour, corps *)
and methode = string * string * ptype * ptype * pterm 
(* Environnements de typage *) 
type env = (string * ptype) list 
(* Listes d'équations *) 
type equa = (ptype * ptype) list
(* pretty printer de termes*)     
let rec print_term (t : pterm) : string =
  match t with
    Var x -> x
    | App (t1, t2) -> "(" ^ (print_term t1) ^" "^ (print_term t2) ^ ")"
    | Abs (x, t) -> "(fun "^ x ^" -> " ^ (print_term t) ^")" 
    | N n -> string_of_int n
    | Add (t1, t2) -> "(" ^ (print_term t1) ^" + "^ (print_term t2) ^ ")"
    | Sub (t1, t2) -> "(" ^ (print_term t1) ^" - "^ (print_term t2) ^ ")"
    | Nil -> "[]"
    | Cons (t1, t2) -> "(" ^ (print_term t1) ^" :: "^ (print_term t2) ^ ")"
    | Hd t1 -> "(hd " ^ (print_term t1) ^ ")"
    | Tl t1 -> "(tl " ^ (print_term t1) ^ ")"
    | IfZero (c, t1, t2) -> "(if zero " ^ (print_term c) ^ " then " ^ (print_term t1) ^ " else " ^ (print_term t2) ^ ")"
    | IfEmpty (c, t1, t2) -> "(if empty " ^ (print_term c) ^ " then " ^ (print_term t1) ^ " else " ^ (print_term t2) ^ ")"
    | Fix t1 -> "(fix " ^ (print_term t1) ^ ")"
    | Let (x, t1, t2) -> "(let " ^ x ^ " = " ^ (print_term t1) ^ " in " ^ (print_term t2) ^ ")"
    | Unit -> "()"
    | Region r -> "rho" ^ (string_of_int r)
    | Ref t1 -> "(ref " ^ (print_term t1) ^ ")"
    | Deref t1 -> "(!" ^ (print_term t1) ^ ")"
    | Assign (t1, t2) -> "(" ^ (print_term t1) ^ " := " ^ (print_term t2) ^ ")"
    | Record fs -> "{" ^ (String.concat "; " (List.map (fun (l, t1) -> l ^ " = " ^ (print_term t1)) fs)) ^ "}"
    | Champ (t1, l) -> (print_term t1) ^ "." ^ l
    | OptNone -> "None"
    | OptSome t1 -> "(Some " ^ (print_term t1) ^ ")"
    | MatchOpt (t1, e1, x, e2) -> "(match " ^ (print_term t1) ^ " with None -> " ^ (print_term e1) ^ " | Some " ^ x ^ " -> " ^ (print_term e2) ^ ")"
    | Raise t1 -> "(raise " ^ (print_term t1) ^ ")"
    | Try (t1, x, t2) -> "(try " ^ (print_term t1) ^ " with " ^ x ^ " -> " ^ (print_term t2) ^ ")"
    | Classe (c, p, attrs, meths, corps) -> "(class " ^ c ^ (match p with None -> "" | Some d -> " extends " ^ d) ^ " = {"
        ^ (String.concat "; " ((List.map (fun (a, ta) -> a ^ " : " ^ (print_type ta)) attrs)
            @ (List.map (fun (m, x, tp, tr, b) -> m ^ " (" ^ x ^ " : " ^ (print_type tp) ^ ") : " ^ (print_type tr) ^ " = " ^ (print_term b)) meths)))
        ^ "} in " ^ (print_term corps) ^ ")"
    | New (c, fs) -> "(new " ^ c ^ " {" ^ (String.concat "; " (List.map (fun (l, t1) -> l ^ " = " ^ (print_term t1)) fs)) ^ "})"
    | Objet (c, fs) -> "<" ^ c ^ " {" ^ (String.concat "; " (List.map (fun (l, t1) -> l ^ " = " ^ (print_term t1)) fs)) ^ "}>"
    | Attribut (t1, a) -> (print_term t1) ^ "#" ^ a
    | Appel (t1, m, a) -> (print_term t1) ^ "#" ^ m ^ "(" ^ (print_term a) ^ ")"
    | AbsT (x, tx, t1) -> "(fun (" ^ x ^ " : " ^ (print_type tx) ^ ") -> " ^ (print_term t1) ^ ")"
    | Coerce (t1, t) -> "(" ^ (print_term t1) ^ " :> " ^ (print_type t) ^ ")"
(* pretty printer de types*)                    
and print_type (t : ptype) : string =
  match t with
    Var x -> x
  | Arr (t1, t2) -> "(" ^ (print_type t1) ^" -> "^ (print_type t2) ^")"
  | Nat -> "Nat"
  | Liste t1 -> "[" ^ (print_type t1) ^ "]"
  | Forall (vs, t1) -> "(forall " ^ (String.concat " " vs) ^ ". " ^ (print_type t1) ^ ")"
  | TUnit -> "unit"
  | TRef t1 -> "(Ref " ^ (print_type t1) ^ ")"
  | TOption t1 -> "(Option " ^ (print_type t1) ^ ")"
  | TRecord (fs, r) -> "{" ^ (String.concat "; " (List.map (fun (l, t1) -> l ^ " : " ^ (print_type t1)) fs))
      ^ (match r with None -> "" | Some x -> " | " ^ x) ^ "}"
  | TClasse c -> c 

(* générateur de noms frais de variables de types *)
let compteur_var : int ref = ref 0                    

let nouvelle_var () : string = compteur_var := !compteur_var + 1; 
  "T"^(string_of_int !compteur_var)


exception VarPasTrouve

(* cherche le type d'une variable dans un environnement *)
let rec cherche_type (v : string) (e : env) : ptype =
  match e with
    [] -> raise VarPasTrouve
  | (v1, t1)::q when v1 = v -> t1
  | (_, _):: q -> (cherche_type v q) 

(* vérificateur d'occurence de variables *)  
let rec appartient_type (v : string) (t : ptype) : bool =
  match t with
    Var v1 when v1 = v -> true
  | Arr (t1, t2) -> (appartient_type v t1) || (appartient_type v t2)
  | Liste t1 -> appartient_type v t1
  | Forall (vs, t1) -> not (List.mem v vs) && (appartient_type v t1)
  | TRef t1 -> appartient_type v t1
  | TOption t1 -> appartient_type v t1
  | TRecord (fs, r) -> r = Some v || List.exists (fun (_, t1) -> appartient_type v t1) fs 
  | _ -> false

(* remplace une variable par un type dans type *)
let rec substitue_type (t : ptype) (v : string) (t0 : ptype) : ptype =
  match t with
    Var v1 when v1 = v -> t0
  | Var v2 -> Var v2
  | Arr (t1, t2) -> Arr (substitue_type t1 v t0, substitue_type t2 v t0)
  | Liste t1 -> Liste (substitue_type t1 v t0)
  | Forall (vs, t1) when List.mem v vs -> Forall (vs, t1)
  | Forall (vs, t1) -> Forall (vs, substitue_type t1 v t0)
  | TRef t1 -> TRef (substitue_type t1 v t0)
  | TUnit -> TUnit
  | TOption t1 -> TOption (substitue_type t1 v t0)
    (* si v est la variable de rangée, on ajoute les champs de t0 *)
  | TRecord (fs, r) ->
      let fs2 : (string * ptype) list = List.map (fun (l, t1) -> (l, substitue_type t1 v t0)) fs in
      if r = Some v then
        (match t0 with
           TRecord (fs3, r3) -> TRecord (List.sort compare (fs2 @ fs3), r3)
         | Var r3 -> TRecord (fs2, Some r3)
         | _ -> TRecord (fs2, r))
      else TRecord (fs2, r)
  | TClasse c -> TClasse c 
  | Nat -> Nat 

(* remplace une variable par un type dans une liste d'équations*)
let substitue_type_partout (e : equa) (v : string) (t0 : ptype) : equa =
  List.map (fun (x, y) -> (substitue_type x v t0, substitue_type y v t0)) e

exception Echec_unif of string

(* table des classes : nom -> (parent, attributs, méthodes) *)
let classes : (string * (string option * (string * ptype) list * methode list)) list ref = ref []

let cherche_classe (c : string) : string option * (string * ptype) list * methode list =
  try List.assoc c !classes with Not_found -> raise (Echec_unif ("classe " ^ c ^ " inconnue"))

(* attributs d'une classe, hérités compris *)
let rec attributs (c : string) : (string * ptype) list =
  match cherche_classe c with
    (None, attrs, _) -> attrs
  | (Some d, attrs, _) -> (attributs d) @ attrs

(* méthodes d'une classe : les siennes d'abord, puis celles des parents *)
let rec methodes (c : string) : methode list =
  match cherche_classe c with
    (None, _, ms) -> ms
  | (Some d, _, ms) -> ms @ (methodes d)

(* distance d'héritage de c1 à c2 (cf Java), -1 si c1 n'est pas une sous-classe de c2 *)
let rec distance (c1 : string) (c2 : string) : int =
  if c1 = c2 then 0
  else match cherche_classe c1 with
      (None, _, _) -> -1
    | (Some d, _, _) -> let n : int = distance d c2 in if n < 0 then -1 else n + 1

(* les méthodes m visibles dans c, une seule par type de paramètre (la redéfinition gagne) *)
let methodes_nommees (c : string) (m : string) : methode list =
  let rec garde (l : methode list) : methode list =
    match l with
      [] -> []
    | ((_, _, tp, _, _) as meth)::q -> meth::(garde (List.filter (fun (_, _, tp2, _, _) -> tp2 <> tp) q)) in
  garde (List.filter (fun (m2, _, _, _, _) -> m2 = m) (methodes c))

(* surcharge : on prend la méthode dont le paramètre est la classe la plus proche de ca *)
let choisit_surcharge (ms : methode list) (ca : string) : methode =
  let score (tp : ptype) : int = match tp with TClasse cp -> distance ca cp | _ -> -1 in
  match List.sort (fun (_, _, tp1, _, _) (_, _, tp2, _, _) -> compare (score tp1) (score tp2))
          (List.filter (fun (_, _, tp, _, _) -> score tp >= 0) ms) with
    [] -> raise (Echec_unif ("pas de methode pour un argument de classe " ^ ca))
  | meth::_ -> meth

(* méthodes surchargées : paramètres de type classe et meme type de retour *)
let verifie_surcharge (ms : methode list) : equa =
  match ms with
    [] | [_] -> []
  | (_, _, _, tr1, _)::_ ->
      if List.exists (fun (_, _, tp, _, _) -> match tp with TClasse _ -> false | _ -> true) ms
      then raise (Echec_unif "surcharge seulement sur des parametres de type classe")
      else List.concat (List.map (fun (_, _, _, tr, _) -> [(tr1, tr); (tr, tr1)]) ms)

(* le nom de classe d'un type objet *)
let classe_de (t : ptype) : string =
  match t with
    TClasse c -> c
  | _ -> raise (Echec_unif ("objet de type inconnu : " ^ (print_type t)))

(* ouvre un forall avec des variables fraiches *)
let rec ouvre (vs : string list) (t : ptype) : ptype =
  match vs with
    [] -> t
  | v::q -> ouvre q (substitue_type t v (Var (nouvelle_var ())))

(* lie la variable x au type t (dans le reste des équations et dans sol) *)
let lie (x : string) (t : ptype) (q : equa) (sol : env) : equa * env =
  if appartient_type x t then raise (Echec_unif ("occurence de " ^ x ^ " dans " ^ (print_type t)))
  else (substitue_type_partout q x t, (x, t)::(List.map (fun (y, ty) -> (y, substitue_type ty x t)) sol))

(* unification de deux records, r1 et r2 sont leurs variables de rangée *)
let unif_records (f1 : (string * ptype) list) (r1 : string option) (f2 : (string * ptype) list) (r2 : string option)
    (q : equa) (sol : env) : equa * env =
  let communs : equa = List.concat (List.map (fun (l, t1) -> try [(t1, List.assoc l f2)] with Not_found -> []) f1) in
  let o1 : (string * ptype) list = List.filter (fun (l, _) -> not (List.mem_assoc l f2)) f1 in
  let o2 : (string * ptype) list = List.filter (fun (l, _) -> not (List.mem_assoc l f1)) f2 in
  let manque (o : (string * ptype) list) : string = "champ(s) " ^ (String.concat ", " (List.map fst o)) ^ " absent(s)" in
  match (r1, r2) with
    (None, None) -> if o1 <> [] then raise (Echec_unif (manque o1))
      else if o2 <> [] then raise (Echec_unif (manque o2)) else (communs @ q, sol)
  | (Some x, None) -> if o1 <> [] then raise (Echec_unif (manque o1)) else lie x (TRecord (o2, None)) (communs @ q) sol
  | (None, Some y) -> if o2 <> [] then raise (Echec_unif (manque o2)) else lie y (TRecord (o1, None)) (communs @ q) sol
  | (Some x, Some y) when x = y -> if o1 = [] && o2 = [] then (communs @ q, sol) else raise (Echec_unif "rangée récursive")
  | (Some x, Some y) -> let r : string = nouvelle_var () in
      let (q1, sol1) = lie x (TRecord (o2, Some r)) (communs @ q) sol in
      lie y (substitue_type (TRecord (o1, Some r)) x (TRecord (o2, Some r))) q1 sol1

(* une étape d'unification, sol : les variables déjà résolues *)
let unif_etape (eqs : equa) (sol : env) : equa * env =
  match eqs with
    [] -> ([], sol)
  | (t1, t2)::q when t1 = t2 -> (q, sol)
  | (Forall (vs, t1), t2)::q -> ((ouvre vs t1, t2)::q, sol)
  | (t1, Forall (vs, t2))::q -> ((t1, ouvre vs t2)::q, sol)
  | (Var x, t)::q -> lie x t q sol
  | (t, Var x)::q -> lie x t q sol
    (* contravariant à gauche de la flèche *)
  | (Arr (t1, t2), Arr (t3, t4))::q -> ((t3, t1)::(t2, t4)::q, sol)
  | (Liste t1, Liste t2)::q -> ((t1, t2)::q, sol)
    (* Ref est invariant *)
  | (TRef t1, TRef t2)::q -> ((t1, t2)::(t2, t1)::q, sol)
  | (TOption t1, TOption t2)::q -> ((t1, t2)::q, sol)
  | (TRecord (f1, r1), TRecord (f2, r2))::q -> unif_records f1 r1 f2 r2 q sol
    (* sous-typage nominal : l'effectif doit etre une sous-classe de l'attendu *)
  | (TClasse c1, TClasse c2)::q -> if distance c2 c1 >= 0 then (q, sol)
      else raise (Echec_unif (c2 ^ " n'est pas une sous-classe de " ^ c1))
  | (t1, t2)::_ -> raise (Echec_unif ((print_type t1) ^ " non-unifiable avec " ^ (print_type t2)))

let max_unif : int = 10000

(* résout un système d'équations avec un timeout *)
let rec resout (eqs : equa) (sol : env) (n : int) : env =
  match eqs with
    [] -> sol
  | _ -> if n <= 0 then raise (Echec_unif "timeout")
      else (let (eqs2, sol2) = unif_etape eqs sol in resout eqs2 sol2 (n - 1))

(* applique la solution à un type *)
let applique_sol (sol : env) (t : ptype) : ptype =
  List.fold_left (fun t1 (x, tx) -> substitue_type t1 x tx) t sol

(* variables libres d'un type *)
let rec vars_libres (t : ptype) : string list =
  match t with
    Var x -> [x]
  | Arr (t1, t2) -> (vars_libres t1) @ (vars_libres t2)
  | Nat -> []
  | Liste t1 -> vars_libres t1
  | TUnit -> []
  | TRef t1 -> vars_libres t1
  | TOption t1 -> vars_libres t1
  | TRecord (fs, r) -> (match r with None -> [] | Some x -> [x]) @ (List.concat (List.map (fun (_, t1) -> vars_libres t1) fs))
  | TClasse _ -> []
  | Forall (vs, t1) -> List.filter (fun x -> not (List.mem x vs)) (vars_libres t1)

(* généralise un type par rapport à un environnement *)
let generalise (t : ptype) (e : env) : ptype =
  let vars_env : string list = List.concat (List.map (fun (_, ty) -> vars_libres ty) e) in
  let vs : string list = List.filter (fun x -> not (List.mem x vars_env)) (vars_libres t) in
  match List.sort_uniq compare vs with
    [] -> t
  | vs2 -> Forall (vs2, t)

(* termes non expansifs (une application est toujours expansive) *)
let rec non_expansif (t : pterm) : bool =
  match t with
    Var _ | N _ | Unit | Nil | OptNone | Abs (_, _) | AbsT (_, _, _) | Objet (_, _) -> true
  | Cons (t1, t2) | Add (t1, t2) | Sub (t1, t2) | Let (_, t1, t2) -> (non_expansif t1) && (non_expansif t2)
  | Hd t1 | Tl t1 -> non_expansif t1
  | IfZero (c, t1, t2) | IfEmpty (c, t1, t2) -> (non_expansif c) && (non_expansif t1) && (non_expansif t2)
  | Fix (Abs (_, t1)) -> non_expansif t1
  | OptSome t1 | Champ (t1, _) -> non_expansif t1
  | Record fs -> List.for_all (fun (_, t1) -> non_expansif t1) fs
  | MatchOpt (t1, e1, _, e2) -> (non_expansif t1) && (non_expansif e1) && (non_expansif e2)
  | New (_, fs) -> List.for_all (fun (_, t1) -> non_expansif t1) fs
  | Classe (_, _, _, _, corps) -> non_expansif corps
  | Attribut (t1, _) | Coerce (t1, _) -> non_expansif t1
  | _ -> false

(* genere des equations de typage à partir d'un terme *)  
let rec genere_equa (te : pterm) (ty : ptype) (e : env) : equa =
  match te with 
    Var v -> let tv : ptype = cherche_type v e in [(ty, tv)] 
  | App (t1, t2) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Arr (Var nv, ty)) e in
      let eq2 : equa = genere_equa t2 (Var nv) e in
      eq1 @ eq2
  | Abs (x, t) -> let nv1 : string = nouvelle_var () 
      and nv2 : string = nouvelle_var () in
      (ty, Arr (Var nv1, Var nv2))::(genere_equa t (Var nv2) ((x, Var nv1)::e))  
  | N _ -> [(ty, Nat)]
  | Add (t1, t2) -> let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat)::(eq1 @ eq2)
  | Sub (t1, t2) -> let eq1 : equa = genere_equa t1 Nat e in
      let eq2 : equa = genere_equa t2 Nat e in
      (ty, Nat)::(eq1 @ eq2)
  | Nil -> [(ty, Liste (Var (nouvelle_var ())))]
  | Cons (t1, t2) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv) e in
      let eq2 : equa = genere_equa t2 (Liste (Var nv)) e in
      (ty, Liste (Var nv))::(eq1 @ eq2)
  | Hd t1 -> genere_equa t1 (Liste ty) e
  | Tl t1 -> let nv : string = nouvelle_var () in
      (ty, Liste (Var nv))::(genere_equa t1 (Liste (Var nv)) e)
  | IfZero (c, t1, t2) -> (genere_equa c Nat e) @ (genere_equa t1 ty e) @ (genere_equa t2 ty e)
  | IfEmpty (c, t1, t2) -> let nv : string = nouvelle_var () in
      (genere_equa c (Liste (Var nv)) e) @ (genere_equa t1 ty e) @ (genere_equa t2 ty e)
  | Fix t1 -> genere_equa t1 (Arr (ty, ty)) e
    (* let : on type t1 tout seul, on généralise si t1 est non expansif, puis t2 *)
  | Let (x, t1, t2) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv) e in
      let sol : env = resout eq1 [] max_unif in
      let t0 : ptype = applique_sol sol (Var nv) in
      let e0 : env = List.map (fun (y, ty0) -> (y, applique_sol sol ty0)) e in
      let tx : ptype = if non_expansif t1 then generalise t0 e0 else t0 in
      eq1 @ (genere_equa t2 ty ((x, tx)::e))
  | Unit -> [(ty, TUnit)]
  | Region _ -> raise (Echec_unif "region dans le terme")
  | Ref t1 -> let nv : string = nouvelle_var () in
      (ty, TRef (Var nv))::(genere_equa t1 (Var nv) e)
  | Deref t1 -> genere_equa t1 (TRef ty) e
  | Assign (t1, t2) -> let nv : string = nouvelle_var () in
      (ty, TUnit)::((genere_equa t1 (TRef (Var nv)) e) @ (genere_equa t2 (Var nv) e))
  | Record fs -> let noms : string list = List.map fst fs in
      if List.length (List.sort_uniq compare noms) <> List.length noms then raise (Echec_unif "champ en double")
      else (let tfs : (string * ptype) list = List.map (fun ((l, _) : string * pterm) : (string * ptype) -> (l, Var (nouvelle_var ()))) fs in
            (ty, TRecord (List.sort compare tfs, None))::(List.concat (List.map2 (fun (_, t1) (_, ty1) -> genere_equa t1 ty1 e) fs tfs)))
    (* r : variable de rangée, le record peut avoir d'autres champs *)
  | Champ (t1, l) -> let r : string = nouvelle_var () in
      genere_equa t1 (TRecord ([(l, ty)], Some r)) e
  | OptNone -> [(ty, TOption (Var (nouvelle_var ())))]
  | OptSome t1 -> let nv : string = nouvelle_var () in
      (ty, TOption (Var nv))::(genere_equa t1 (Var nv) e)
  | MatchOpt (t1, e1, x, e2) -> let nv : string = nouvelle_var () in
      (genere_equa t1 (TOption (Var nv)) e) @ (genere_equa e1 ty e) @ (genere_equa e2 ty ((x, Var nv)::e))
    (* une exception porte un entier, raise e peut avoir n'importe quel type *)
  | Raise t1 -> genere_equa t1 Nat e
  | Try (t1, x, t2) -> (genere_equa t1 ty e) @ (genere_equa t2 ty ((x, Nat)::e))
    (* classe : on l'enregistre, on vérifie les méthodes (avec self : c), puis le corps *)
  | Classe (c, p, attrs, meths, corps) ->
      (match p with Some d -> ignore (cherche_classe d) | None -> ());
      classes := (c, (p, attrs, meths))::(List.remove_assoc c !classes);
      let noms : string list = List.map fst (attributs c) in
      if List.length (List.sort_uniq compare noms) <> List.length noms then raise (Echec_unif "attribut en double")
      else
        let eq_meths : equa = List.concat (List.map (fun (_, x, tp, tr, b) ->
            genere_equa b tr (("self", TClasse c)::(x, tp)::e)) meths) in
        (* redéfinition (meme paramètre qu'un parent) : le type de retour doit etre un sous-type *)
        let parents : methode list = (match p with Some d -> methodes d | None -> []) in
        let eq_redef : equa = List.concat (List.map (fun (m, _, tp, tr, _) ->
            List.concat (List.map (fun (m2, _, tp2, tr2, _) -> if m2 = m && tp2 = tp then [(tr2, tr)] else []) parents)) meths) in
        let eq_surcharge : equa = List.concat (List.map (fun (m, _, _, _, _) -> verifie_surcharge (methodes_nommees c m)) meths) in
        eq_meths @ eq_redef @ eq_surcharge @ (genere_equa corps ty e)
  | New (c, fs) -> let attrs : (string * ptype) list = attributs c in
      if List.sort compare (List.map fst fs) <> List.sort compare (List.map fst attrs)
      then raise (Echec_unif ("new " ^ c ^ " : il faut les attributs " ^ (String.concat ", " (List.map fst attrs))))
      else (ty, TClasse c)::(List.concat (List.map (fun (a, t1) -> genere_equa t1 (List.assoc a attrs) e) fs))
  | Objet _ -> raise (Echec_unif "objet dans le terme")
    (* pour un attribut ou une méthode il faut la classe de l'objet : on le type tout seul *)
  | Attribut (t1, a) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv) e in
      let c : string = classe_de (applique_sol (resout eq1 [] max_unif) (Var nv)) in
      (match List.assoc_opt a (attributs c) with
         Some ta -> (ty, ta)::eq1
       | None -> raise (Echec_unif ("pas d'attribut " ^ a ^ " dans " ^ c)))
  | Appel (t1, m, a) -> let nv : string = nouvelle_var () in
      let eq1 : equa = genere_equa t1 (Var nv) e in
      let c : string = classe_de (applique_sol (resout eq1 [] max_unif) (Var nv)) in
      (match methodes_nommees c m with
         [] -> raise (Echec_unif ("pas de methode " ^ m ^ " dans " ^ c))
       | [(_, _, tp, tr, _)] -> (ty, tr)::(eq1 @ (genere_equa a tp e))
       | ms -> let nv2 : string = nouvelle_var () in
           let eq2 : equa = genere_equa a (Var nv2) e in
           let (_, _, tp, tr, _) = choisit_surcharge ms (classe_de (applique_sol (resout eq2 [] max_unif) (Var nv2))) in
           (ty, tr)::(tp, Var nv2)::(eq1 @ eq2))
  | AbsT (x, tx, t1) -> let nv : string = nouvelle_var () in
      (ty, Arr (tx, Var nv))::(genere_equa t1 (Var nv) ((x, tx)::e))
  | Coerce (t1, t) -> (ty, t)::(genere_equa t1 t e)
      
(* enchaine generation d'equation et unification *)
let inference (t : pterm) : string =
  classes := [];
  let but : string = nouvelle_var () in
  try (let sol : env = resout (genere_equa t (Var but) []) [] max_unif in
       (print_term t)^" ***TYPABLE*** avec le type "^(print_type (applique_sol sol (Var but))))
  with Echec_unif bla -> (print_term t)^" ***PAS TYPABLE*** : "^bla
     | VarPasTrouve -> (print_term t)^" ***PAS TYPABLE*** : variable libre"


(* noms frais pour les variables de termes : x -> x_1, y -> y_2, ... *)
(* (on enleve l'ancien numero pour que les noms ne grossissent pas) *)
let compteur_var_t : int ref = ref 0

let nouvelle_var_t (x : string) : string =
  compteur_var_t := !compteur_var_t + 1;
  let base : string = (try String.sub x 0 (String.index x '_') with Not_found -> x) in
  base ^ "_" ^ (string_of_int !compteur_var_t)

(* alpha-conversion : renomme toutes les variables liées par des variables fraiches *)
(* renom : liste (ancien nom, nouveau nom) des variables liées au dessus *)
let rec alphaconv_aux (t : pterm) (renom : (string * string) list) : pterm =
  match t with
    Var x -> (try Var (List.assoc x renom) with Not_found -> Var x)
  | Abs (x, t1) -> let nx : string = nouvelle_var_t x in
      Abs (nx, alphaconv_aux t1 ((x, nx)::renom))
  | App (t1, t2) -> App (alphaconv_aux t1 renom, alphaconv_aux t2 renom)
  | N n -> N n
  | Add (t1, t2) -> Add (alphaconv_aux t1 renom, alphaconv_aux t2 renom)
  | Sub (t1, t2) -> Sub (alphaconv_aux t1 renom, alphaconv_aux t2 renom)
  | Nil -> Nil
  | Cons (t1, t2) -> Cons (alphaconv_aux t1 renom, alphaconv_aux t2 renom)
  | Hd t1 -> Hd (alphaconv_aux t1 renom)
  | Tl t1 -> Tl (alphaconv_aux t1 renom)
  | IfZero (c, t1, t2) -> IfZero (alphaconv_aux c renom, alphaconv_aux t1 renom, alphaconv_aux t2 renom)
  | IfEmpty (c, t1, t2) -> IfEmpty (alphaconv_aux c renom, alphaconv_aux t1 renom, alphaconv_aux t2 renom)
  | Fix t1 -> Fix (alphaconv_aux t1 renom)
  | Let (x, t1, t2) -> let nx : string = nouvelle_var_t x in
      Let (nx, alphaconv_aux t1 renom, alphaconv_aux t2 ((x, nx)::renom))
  | Unit -> Unit
  | Region r -> Region r
  | Ref t1 -> Ref (alphaconv_aux t1 renom)
  | Deref t1 -> Deref (alphaconv_aux t1 renom)
  | Assign (t1, t2) -> Assign (alphaconv_aux t1 renom, alphaconv_aux t2 renom)
  | Record fs -> Record (List.map (fun (l, t1) -> (l, alphaconv_aux t1 renom)) fs)
  | Champ (t1, l) -> Champ (alphaconv_aux t1 renom, l)
  | OptNone -> OptNone
  | OptSome t1 -> OptSome (alphaconv_aux t1 renom)
  | MatchOpt (t1, e1, x, e2) -> let nx : string = nouvelle_var_t x in
      MatchOpt (alphaconv_aux t1 renom, alphaconv_aux e1 renom, nx, alphaconv_aux e2 ((x, nx)::renom))
  | Raise t1 -> Raise (alphaconv_aux t1 renom)
  | Try (t1, x, t2) -> let nx : string = nouvelle_var_t x in
      Try (alphaconv_aux t1 renom, nx, alphaconv_aux t2 ((x, nx)::renom))
  | Classe (c, p, attrs, meths, corps) ->
      Classe (c, p, attrs, List.map (fun (m, y, tp, tr, b) -> let ny : string = nouvelle_var_t y in
          (m, ny, tp, tr, alphaconv_aux b ((y, ny)::(List.remove_assoc "self" renom)))) meths, alphaconv_aux corps renom)
  | New (c, fs) -> New (c, List.map (fun (l, t1) -> (l, alphaconv_aux t1 renom)) fs)
  | Objet (c, fs) -> Objet (c, fs)
  | Attribut (t1, a) -> Attribut (alphaconv_aux t1 renom, a)
  | Appel (t1, m, a) -> Appel (alphaconv_aux t1 renom, m, alphaconv_aux a renom)
  | AbsT (x, tx, t1) -> let nx : string = nouvelle_var_t x in
      AbsT (nx, tx, alphaconv_aux t1 ((x, nx)::renom))
  | Coerce (t1, t) -> Coerce (alphaconv_aux t1 renom, t)

let alphaconv (t : pterm) : pterm = alphaconv_aux t []

(* substitution : remplace la variable x par n dans t (t[n/x]) *)
let rec substitution (x : string) (n : pterm) (t : pterm) : pterm =
  match t with
    Var y when y = x -> n
  | Var y -> Var y
  | Abs (y, t1) when y = x -> Abs (y, t1) (* x est re-liée ici donc on s'arrete *)
  | Abs (y, t1) -> Abs (y, substitution x n t1)
  | App (t1, t2) -> App (substitution x n t1, substitution x n t2)
  | N k -> N k
  | Add (t1, t2) -> Add (substitution x n t1, substitution x n t2)
  | Sub (t1, t2) -> Sub (substitution x n t1, substitution x n t2)
  | Nil -> Nil
  | Cons (t1, t2) -> Cons (substitution x n t1, substitution x n t2)
  | Hd t1 -> Hd (substitution x n t1)
  | Tl t1 -> Tl (substitution x n t1)
  | IfZero (c, t1, t2) -> IfZero (substitution x n c, substitution x n t1, substitution x n t2)
  | IfEmpty (c, t1, t2) -> IfEmpty (substitution x n c, substitution x n t1, substitution x n t2)
  | Fix t1 -> Fix (substitution x n t1)
  | Let (y, t1, t2) when y = x -> Let (y, substitution x n t1, t2)
  | Let (y, t1, t2) -> Let (y, substitution x n t1, substitution x n t2)
  | Unit -> Unit
  | Region r -> Region r
  | Ref t1 -> Ref (substitution x n t1)
  | Deref t1 -> Deref (substitution x n t1)
  | Assign (t1, t2) -> Assign (substitution x n t1, substitution x n t2)
  | Record fs -> Record (List.map (fun (l, t1) -> (l, substitution x n t1)) fs)
  | Champ (t1, l) -> Champ (substitution x n t1, l)
  | OptNone -> OptNone
  | OptSome t1 -> OptSome (substitution x n t1)
  | MatchOpt (t1, e1, y, e2) when y = x -> MatchOpt (substitution x n t1, substitution x n e1, y, e2)
  | MatchOpt (t1, e1, y, e2) -> MatchOpt (substitution x n t1, substitution x n e1, y, substitution x n e2)
  | Raise t1 -> Raise (substitution x n t1)
  | Try (t1, y, t2) when y = x -> Try (substitution x n t1, y, t2)
  | Try (t1, y, t2) -> Try (substitution x n t1, y, substitution x n t2)
  | Classe (c, p, attrs, meths, corps) ->
      Classe (c, p, attrs, List.map (fun (m, y, tp, tr, b) ->
          if x = "self" || x = y then (m, y, tp, tr, b) else (m, y, tp, tr, substitution x n b)) meths, substitution x n corps)
  | New (c, fs) -> New (c, List.map (fun (l, t1) -> (l, substitution x n t1)) fs)
  | Objet (c, fs) -> Objet (c, fs)
  | Attribut (t1, a) -> Attribut (substitution x n t1, a)
  | Appel (t1, m, a) -> Appel (substitution x n t1, m, substitution x n a)
  | AbsT (y, ty1, t1) when y = x -> AbsT (y, ty1, t1)
  | AbsT (y, ty1, t1) -> AbsT (y, ty1, substitution x n t1)
  | Coerce (t1, t) -> Coerce (substitution x n t1, t)

(* valeurs (cf cours) : variables, abstractions, entiers, et x V (application bloquée) *)
let rec est_valeur (t : pterm) : bool =
  match t with
    Var _ -> true
  | Abs (_, _) -> true
  | N _ -> true
  | App (Var _, v) -> est_valeur v
  | Nil -> true
  | Cons (t1, t2) -> (est_valeur t1) && (est_valeur t2)
  | Unit -> true
  | Region _ -> true
  | Record fs -> List.for_all (fun (_, t1) -> est_valeur t1) fs
  | OptNone -> true
  | OptSome t1 -> est_valeur t1
  | AbsT (_, _, _) -> true
  | Objet (_, _) -> true
  | _ -> false

(* levée quand on évalue raise v, rattrapée par le try le plus proche *)
exception Levee of pterm

(* la mémoire : région -> valeur *)
let memoire : (int * pterm) list ref = ref []
let compteur_region : int ref = ref 0

(* affiche la mémoire (rien si elle est vide) *)
let print_memoire (m : (int * pterm) list) : string =
  if m = [] then ""
  else "   ; " ^ (String.concat ", " (List.map (fun (r, v) -> "rho" ^ (string_of_int r) ^ " -> " ^ (print_term v)) (List.sort compare m)))

(* une étape de réduction en Left-to-Right Call-by-Value *)
(* None si on ne peut plus réduire (valeur ou terme bloqué) *)
let rec ltr_cbv_step (t : pterm) : pterm option =
  match t with
    (* d'abord la fonction *)
    App (t1, t2) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (App (t1', t2))
       | None -> None)
    (* puis l'argument *)
  | App (t1, t2) when not (est_valeur t2) ->
      (match ltr_cbv_step t2 with
         Some t2' -> Some (App (t1, t2'))
       | None -> None)
    (* beta-réduction : on alpha-convertit le corps avant de substituer (pas de capture) *)
  | App (Abs (x, t1), v) -> Some (substitution x v (alphaconv t1))
  | App (AbsT (x, _, t1), v) -> Some (substitution x v (alphaconv t1))
    (* addition : gauche, puis droite, puis on calcule *)
  | Add (t1, t2) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Add (t1', t2))
       | None -> None)
  | Add (t1, t2) when not (est_valeur t2) ->
      (match ltr_cbv_step t2 with
         Some t2' -> Some (Add (t1, t2'))
       | None -> None)
  | Add (N n1, N n2) -> Some (N (n1 + n2))
  | Sub (t1, t2) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Sub (t1', t2))
       | None -> None)
  | Sub (t1, t2) when not (est_valeur t2) ->
      (match ltr_cbv_step t2 with
         Some t2' -> Some (Sub (t1, t2'))
       | None -> None)
  | Sub (N n1, N n2) -> Some (N (n1 - n2))
  | Cons (t1, t2) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Cons (t1', t2))
       | None -> None)
  | Cons (t1, t2) when not (est_valeur t2) ->
      (match ltr_cbv_step t2 with
         Some t2' -> Some (Cons (t1, t2'))
       | None -> None)
  | Hd t1 when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Hd t1')
       | None -> None)
  | Hd (Cons (v1, _)) -> Some v1
  | Tl t1 when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Tl t1')
       | None -> None)
  | Tl (Cons (_, v2)) -> Some v2
    (* if : on réduit seulement la condition *)
  | IfZero (c, t1, t2) when not (est_valeur c) ->
      (match ltr_cbv_step c with
         Some c' -> Some (IfZero (c', t1, t2))
       | None -> None)
  | IfZero (N 0, t1, _) -> Some t1
  | IfZero (N _, _, t2) -> Some t2
  | IfEmpty (c, t1, t2) when not (est_valeur c) ->
      (match ltr_cbv_step c with
         Some c' -> Some (IfEmpty (c', t1, t2))
       | None -> None)
  | IfEmpty (Nil, t1, _) -> Some t1
  | IfEmpty (Cons (_, _), _, t2) -> Some t2
  | Fix t1 when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Fix t1')
       | None -> None)
  | Fix (Abs (phi, m)) -> Some (substitution phi (Fix (Abs (phi, m))) (alphaconv m))
  | Let (x, t1, t2) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Let (x, t1', t2))
       | None -> None)
  | Let (x, v, t2) -> Some (substitution x v (alphaconv t2))
  | Ref t1 when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Ref t1')
       | None -> None)
  | Ref v -> compteur_region := !compteur_region + 1;
      memoire := (!compteur_region, v)::!memoire;
      Some (Region !compteur_region)
  | Deref t1 when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Deref t1')
       | None -> None)
  | Deref (Region r) -> (try Some (List.assoc r !memoire) with Not_found -> None)
  | Assign (t1, t2) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Assign (t1', t2))
       | None -> None)
  | Assign (t1, t2) when not (est_valeur t2) ->
      (match ltr_cbv_step t2 with
         Some t2' -> Some (Assign (t1, t2'))
       | None -> None)
  | Assign (Region r, v) -> memoire := (r, v)::(List.remove_assoc r !memoire); Some Unit
  | Record fs when not (est_valeur t) ->
      (match etape_champs fs with
         Some fs' -> Some (Record fs')
       | None -> None)
  | Champ (t1, l) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Champ (t1', l))
       | None -> None)
  | Champ (Record fs, l) -> (try Some (List.assoc l fs) with Not_found -> None)
  | OptSome t1 when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (OptSome t1')
       | None -> None)
  | MatchOpt (t1, e1, x, e2) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (MatchOpt (t1', e1, x, e2))
       | None -> None)
  | MatchOpt (OptNone, e1, _, _) -> Some e1
  | MatchOpt (OptSome v, _, x, e2) -> Some (substitution x v (alphaconv e2))
  | Raise t1 when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Raise t1')
       | None -> None)
  | Raise v -> raise (Levee v)
  | Try (t1, x, t2) when not (est_valeur t1) ->
      (try (match ltr_cbv_step t1 with
              Some t1' -> Some (Try (t1', x, t2))
            | None -> None)
       with Levee v -> Some (substitution x v (alphaconv t2)))
  | Try (v, _, _) -> Some v
  | Classe (c, p, attrs, meths, corps) -> classes := (c, (p, attrs, meths))::(List.remove_assoc c !classes); Some corps
  | New (c, fs) when not (List.for_all (fun (_, t1) -> est_valeur t1) fs) ->
      (match etape_champs fs with
         Some fs' -> Some (New (c, fs'))
       | None -> None)
  | New (c, fs) -> Some (Objet (c, fs))
  | Attribut (t1, a) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Attribut (t1', a))
       | None -> None)
  | Attribut (Objet (_, fs), a) -> List.assoc_opt a fs
  | Appel (t1, m, a) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Appel (t1', m, a))
       | None -> None)
  | Appel (t1, m, a) when not (est_valeur a) ->
      (match ltr_cbv_step a with
         Some a' -> Some (Appel (t1, m, a'))
       | None -> None)
    (* appel de méthode : on cherche la méthode dans la vraie classe de l'objet *)
  | Appel (Objet (c, fs), m, v) ->
      (try
         let (_, x, _, _, corps) = (match (methodes_nommees c m, v) with
               ([meth], _) -> meth
             | (ms, Objet (ca, _)) -> choisit_surcharge ms ca
             | _ -> raise Not_found) in
         Some (substitution x v (substitution "self" (Objet (c, fs)) (alphaconv corps)))
       with Not_found | Echec_unif _ -> None)
  | Coerce (t1, t) when not (est_valeur t1) ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some (Coerce (t1', t))
       | None -> None)
  | Coerce (v, _) -> Some v
    (* le reste : valeurs (on ne réduit pas sous les lambdas) ou termes bloqués *)
  | _ -> None

(* réduit le premier champ qui n'est pas encore une valeur *)
and etape_champs (fs : (string * pterm) list) : (string * pterm) list option =
  match fs with
    [] -> None
  | (l, t1)::q when est_valeur t1 ->
      (match etape_champs q with
         Some q' -> Some ((l, t1)::q')
       | None -> None)
  | (l, t1)::q ->
      (match ltr_cbv_step t1 with
         Some t1' -> Some ((l, t1')::q)
       | None -> None)

exception Timeout

(* réduit t jusqu'au bout en affichant chaque réduit, au plus n étapes sinon Timeout *)
let rec ltr_cbv_norm (t : pterm) (n : int) : pterm =
  match ltr_cbv_step t with
    None -> t
  | Some t' -> if n <= 0 then raise Timeout
      else (print_endline ("--> " ^ (print_term t') ^ (print_memoire !memoire)); ltr_cbv_norm t' (n - 1))

(* affiche toutes les réductions d'un terme, avec un timeout de limite étapes *)
let evaluation (t : pterm) (limite : int) : unit =
  memoire := [];
  compteur_region := 0;
  classes := [];
  print_endline (print_term t);
  try
    let res : pterm = ltr_cbv_norm t limite in
    if est_valeur res then print_endline ("***VALEUR*** " ^ (print_term res))
    else print_endline ("***BLOQUE*** " ^ (print_term res))
  with Timeout -> print_endline ("***TIMEOUT*** apres " ^ (string_of_int limite) ^ " etapes")
     | Levee v -> print_endline ("***EXCEPTION*** non rattrapee : " ^ (print_term v))

(* tokens du parser *)
type token = Mot of string | Maj of string | Nombre of int | Sym of string | Fin

exception Erreur_syntaxe of string

let print_token (t : token) : string =
  match t with
    Mot m -> m
  | Maj m -> m
  | Nombre n -> string_of_int n
  | Sym s -> s
  | Fin -> "la fin du texte"

(* lexer : découpe le texte en tokens, les commentaires (* ... *) sont sautés *)
let lexer (s : string) : token list =
  let n : int = String.length s in
  let est_chiffre (c : char) : bool = c >= '0' && c <= '9' in
  let est_lettre (c : char) : bool = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_' in
  let rec fin_mot (i : int) : int =
    if i < n && (est_lettre s.[i] || est_chiffre s.[i] || s.[i] = '\'') then fin_mot (i + 1) else i in
  let rec fin_nombre (i : int) : int = if i < n && est_chiffre s.[i] then fin_nombre (i + 1) else i in
  let rec fin_commentaire (i : int) : int =
    if i + 1 >= n then raise (Erreur_syntaxe "commentaire pas fermé")
    else if s.[i] = '*' && s.[i + 1] = ')' then i + 2 else fin_commentaire (i + 1) in
  let rec lit (i : int) (acc : token list) : token list =
    if i >= n then List.rev (Fin::acc)
    else
      let c : char = s.[i] in
      if c = ' ' || c = '\n' || c = '\t' || c = '\r' then lit (i + 1) acc
      else if c = '(' && i + 1 < n && s.[i + 1] = '*' then lit (fin_commentaire (i + 2)) acc
      else if est_chiffre c then
        (let j : int = fin_nombre i in lit j ((Nombre (int_of_string (String.sub s i (j - i))))::acc))
      else if est_lettre c then
        (let j : int = fin_mot i in
         let m : string = String.sub s i (j - i) in
         lit j ((if c >= 'A' && c <= 'Z' then Maj m else Mot m)::acc))
      else if i + 1 < n && List.mem (String.sub s i 2) ["->"; "::"; ":="; ":>"] then lit (i + 2) ((Sym (String.sub s i 2))::acc)
      else if String.contains "()[]{}=+-:;!.#|" c then lit (i + 1) ((Sym (String.make 1 c))::acc)
      else raise (Erreur_syntaxe ("caractère inconnu : " ^ (String.make 1 c))) in
  lit 0 []

let mots_cles : string list = ["fun"; "let"; "rec"; "in"; "if"; "zero"; "empty"; "then"; "else"; "match"; "with";
  "try"; "raise"; "hd"; "tl"; "fix"; "ref"; "class"; "extends"; "new"]

(* enlève le symbole ou le mot-clé s au début des tokens *)
let attend (s : string) (toks : token list) : token list =
  match toks with
    (Sym s2 | Mot s2)::q when s2 = s -> q
  | t::_ -> raise (Erreur_syntaxe ("on attendait " ^ s ^ " mais on a " ^ (print_token t)))
  | [] -> raise (Erreur_syntaxe ("on attendait " ^ s))

let saute_pv (toks : token list) : token list = match toks with Sym ";"::q -> q | _ -> toks

(* est-ce que les tokens commencent un argument (pour l'application) ? *)
let commence_atome (toks : token list) : bool =
  match toks with
    Nombre _::_ -> true
  | Mot m::_ -> not (List.mem m mots_cles) || List.mem m ["hd"; "tl"; "ref"; "raise"; "fix"; "new"]
  | Maj m::_ -> m = "None" || m = "Some"
  | Sym s::_ -> List.mem s ["("; "["; "{"; "!"]
  | _ -> false

(* les paramètres x y z avant = ou -> *)
let rec lit_params (toks : token list) : string list * token list =
  match toks with
    Mot x::q when not (List.mem x mots_cles) -> let (xs, q1) = lit_params q in (x::xs, q1)
  | _ -> ([], toks)

(* fun x y -> e devient fun x -> fun y -> e *)
let fonction (xs : string list) (e : pterm) : pterm = List.fold_right (fun x t -> Abs (x, t)) xs e

(* types : T -> T, Nat, unit, [T], Ref T, Option T, {l : T; ...}, nom de classe *)
let rec lit_type (toks : token list) : ptype * token list =
  let (t1, q) = lit_type_atome toks in
  match q with
    Sym "->"::q1 -> let (t2, q2) = lit_type q1 in (Arr (t1, t2), q2)
  | _ -> (t1, q)

and lit_type_atome (toks : token list) : ptype * token list =
  match toks with
    Maj "Nat"::q -> (Nat, q)
  | Mot "unit"::q -> (TUnit, q)
  | Maj "Ref"::q -> let (t, q1) = lit_type_atome q in (TRef t, q1)
  | Maj "Option"::q -> let (t, q1) = lit_type_atome q in (TOption t, q1)
  | Maj c::q -> (TClasse c, q)
  | Sym "["::q -> let (t, q1) = lit_type q in (Liste t, attend "]" q1)
  | Sym "("::q -> let (t, q1) = lit_type q in (t, attend ")" q1)
  | Sym "{"::q -> let (fs, q1) = lit_champs_types q in (TRecord (fs, None), q1)
  | t::_ -> raise (Erreur_syntaxe ("on attendait un type mais on a " ^ (print_token t)))
  | [] -> raise (Erreur_syntaxe "on attendait un type")

and lit_champs_types (toks : token list) : (string * ptype) list * token list =
  match toks with
    Sym "}"::q -> ([], q)
  | Mot l::Sym ":"::q ->
      let (t, q1) = lit_type q in
      (match q1 with
         Sym ";"::q2 -> let (fs, q3) = lit_champs_types q2 in ((l, t)::fs, q3)
       | Sym "}"::q2 -> ([(l, t)], q2)
       | _ -> raise (Erreur_syntaxe "on attendait ; ou }"))
  | _ -> raise (Erreur_syntaxe "on attendait un champ l : T")

(* parser : une fonction par niveau de priorité, chacune renvoie le terme lu et les tokens qui restent *)
let rec lit_expr (toks : token list) : pterm * token list =
  match toks with
    Mot "fun"::Sym "("::Mot x::Sym ":"::q ->
      let (t, q1) = lit_type q in
      let (e, q2) = lit_expr (attend "->" (attend ")" q1)) in
      (AbsT (x, t, e), q2)
  | Mot "fun"::q ->
      let (xs, q1) = lit_params q in
      if xs = [] then raise (Erreur_syntaxe "fun : il manque le paramètre")
      else (let (e, q2) = lit_expr (attend "->" q1) in (fonction xs e, q2))
    (* let rec f x = e1 in e2 devient let f = fix (fun f -> fun x -> e1) in e2 *)
  | Mot "let"::Mot "rec"::Mot f::q ->
      let (xs, q1) = lit_params q in
      let (e1, q2) = lit_expr (attend "=" q1) in
      let (e2, q3) = lit_expr (attend "in" q2) in
      (Let (f, Fix (Abs (f, fonction xs e1)), e2), q3)
  | Mot "let"::Mot x::q ->
      let (xs, q1) = lit_params q in
      let (e1, q2) = lit_expr (attend "=" q1) in
      let (e2, q3) = lit_expr (attend "in" q2) in
      (Let (x, fonction xs e1, e2), q3)
  | Mot "if"::Mot "zero"::q ->
      let (c, q1) = lit_expr q in
      let (e1, q2) = lit_expr (attend "then" q1) in
      let (e2, q3) = lit_expr (attend "else" q2) in
      (IfZero (c, e1, e2), q3)
  | Mot "if"::Mot "empty"::q ->
      let (c, q1) = lit_expr q in
      let (e1, q2) = lit_expr (attend "then" q1) in
      let (e2, q3) = lit_expr (attend "else" q2) in
      (IfEmpty (c, e1, e2), q3)
  | Mot "match"::q ->
      let (e, q1) = lit_expr q in
      (match attend "with" q1 with
         Maj "None"::Sym "->"::q2 ->
           let (e1, q3) = lit_expr q2 in
           (match q3 with
              Sym "|"::Maj "Some"::Mot x::Sym "->"::q4 -> let (e2, q5) = lit_expr q4 in (MatchOpt (e, e1, x, e2), q5)
            | _ -> raise (Erreur_syntaxe "match : on attendait | Some x ->"))
       | _ -> raise (Erreur_syntaxe "match : on attendait None ->"))
  | Mot "try"::q ->
      let (e1, q1) = lit_expr q in
      (match attend "with" q1 with
         Mot x::Sym "->"::q2 -> let (e2, q3) = lit_expr q2 in (Try (e1, x, e2), q3)
       | _ -> raise (Erreur_syntaxe "try : on attendait with x ->"))
  | Mot "class"::Maj c::q ->
      let (parent, q1) = (match q with Mot "extends"::Maj d::q2 -> (Some d, q2) | _ -> (None, q)) in
      let (attrs, meths, q2) = lit_membres (attend "{" (attend "=" q1)) in
      let (corps, q3) = lit_expr (attend "in" q2) in
      (Classe (c, parent, attrs, meths, corps), q3)
  | _ -> lit_affect toks

and lit_affect (toks : token list) : pterm * token list =
  let (e1, q1) = lit_cons toks in
  match q1 with
    Sym ":="::q2 -> let (e2, q3) = lit_expr q2 in (Assign (e1, e2), q3)
  | _ -> (e1, q1)

and lit_cons (toks : token list) : pterm * token list =
  let (e1, q1) = lit_arith toks in
  match q1 with
    Sym "::"::q2 -> let (e2, q3) = lit_cons q2 in (Cons (e1, e2), q3)
  | _ -> (e1, q1)

and lit_arith (toks : token list) : pterm * token list =
  let (e1, q1) = lit_app toks in lit_arith_suite e1 q1

and lit_arith_suite (e1 : pterm) (toks : token list) : pterm * token list =
  match toks with
    Sym "+"::q -> let (e2, q1) = lit_app q in lit_arith_suite (Add (e1, e2)) q1
  | Sym "-"::q -> let (e2, q1) = lit_app q in lit_arith_suite (Sub (e1, e2)) q1
  | _ -> (e1, toks)

and lit_app (toks : token list) : pterm * token list =
  let (e1, q1) = lit_unaire toks in lit_app_suite e1 q1

and lit_app_suite (e1 : pterm) (toks : token list) : pterm * token list =
  if commence_atome toks then (let (e2, q1) = lit_unaire toks in lit_app_suite (App (e1, e2)) q1)
  else (e1, toks)

and lit_unaire (toks : token list) : pterm * token list =
  match toks with
    Mot "hd"::q -> let (e, q1) = lit_unaire q in (Hd e, q1)
  | Mot "tl"::q -> let (e, q1) = lit_unaire q in (Tl e, q1)
  | Mot "ref"::q -> let (e, q1) = lit_unaire q in (Ref e, q1)
  | Mot "raise"::q -> let (e, q1) = lit_unaire q in (Raise e, q1)
  | Mot "fix"::q -> let (e, q1) = lit_unaire q in (Fix e, q1)
  | Maj "Some"::q -> let (e, q1) = lit_unaire q in (OptSome e, q1)
  | Sym "!"::q -> let (e, q1) = lit_unaire q in (Deref e, q1)
  | _ -> lit_postfix toks

and lit_postfix (toks : token list) : pterm * token list =
  let (e, q) = lit_atome toks in lit_postfix_suite e q

and lit_postfix_suite (e : pterm) (toks : token list) : pterm * token list =
  match toks with
    Sym "."::Mot l::q -> lit_postfix_suite (Champ (e, l)) q
  | Sym "#"::Mot m::Sym "("::Sym ")"::q -> lit_postfix_suite (Appel (e, m, Unit)) q
  | Sym "#"::Mot m::Sym "("::q -> let (a, q1) = lit_expr q in lit_postfix_suite (Appel (e, m, a)) (attend ")" q1)
  | Sym "#"::Mot m::q -> lit_postfix_suite (Attribut (e, m)) q
  | _ -> (e, toks)

and lit_atome (toks : token list) : pterm * token list =
  match toks with
    Nombre k::q -> (N k, q)
  | Mot x::q when not (List.mem x mots_cles) -> (Var x, q)
  | Maj "None"::q -> (OptNone, q)
  | Mot "new"::Maj c::Sym "{"::q -> let (fs, q1) = lit_champs q in (New (c, fs), q1)
  | Sym "("::Sym ")"::q -> (Unit, q)
  | Sym "("::q ->
      let (e, q1) = lit_expr q in
      (match q1 with
         Sym ")"::q2 -> (e, q2)
       | Sym ":>"::q2 -> let (t, q3) = lit_type q2 in (Coerce (e, t), attend ")" q3)
       | t::_ -> raise (Erreur_syntaxe ("on attendait ) mais on a " ^ (print_token t)))
       | [] -> raise (Erreur_syntaxe "on attendait )"))
  | Sym "["::Sym "]"::q -> (Nil, q)
  | Sym "["::q -> lit_liste q
  | Sym "{"::q -> let (fs, q1) = lit_champs q in (Record fs, q1)
  | t::_ -> raise (Erreur_syntaxe ("on ne s'attendait pas à " ^ (print_token t)))
  | [] -> raise (Erreur_syntaxe "texte vide")

and lit_champs (toks : token list) : (string * pterm) list * token list =
  match toks with
    Sym "}"::q -> ([], q)
  | Mot l::Sym "="::q ->
      let (e, q1) = lit_expr q in
      (match q1 with
         Sym ";"::q2 -> let (fs, q3) = lit_champs q2 in ((l, e)::fs, q3)
       | Sym "}"::q2 -> ([(l, e)], q2)
       | _ -> raise (Erreur_syntaxe "on attendait ; ou }"))
  | _ -> raise (Erreur_syntaxe "on attendait un champ l = e")

(* [e1; e2; e3] devient e1 :: e2 :: e3 :: [] *)
and lit_liste (toks : token list) : pterm * token list =
  let (e, q) = lit_expr toks in
  match q with
    Sym ";"::q1 -> let (reste, q2) = lit_liste q1 in (Cons (e, reste), q2)
  | Sym "]"::q1 -> (Cons (e, Nil), q1)
  | _ -> raise (Erreur_syntaxe "on attendait ; ou ]")

(* membres d'une classe : a : T ou m (x : T) : T = e, séparés par des ; *)
and lit_membres (toks : token list) : (string * ptype) list * methode list * token list =
  match toks with
    Sym "}"::q -> ([], [], q)
  | Mot a::Sym ":"::q ->
      let (t, q1) = lit_type q in
      let (attrs, meths, q2) = lit_membres (saute_pv q1) in
      ((a, t)::attrs, meths, q2)
  | Mot m::Sym "("::Mot x::Sym ":"::q ->
      let (tp, q1) = lit_type q in
      let (tr, q2) = lit_type (attend ":" (attend ")" q1)) in
      let (corps, q3) = lit_expr (attend "=" q2) in
      let (attrs, meths, q4) = lit_membres (saute_pv q3) in
      (attrs, (m, x, tp, tr, corps)::meths, q4)
  | _ -> raise (Erreur_syntaxe "on attendait un attribut a : T ou une methode m (x : T) : T = e")

(* lit un programme complet *)
let lit_programme (s : string) : pterm =
  let (e, q) = lit_expr (lexer s) in
  match q with
    [Fin] | [] -> e
  | t::_ -> raise (Erreur_syntaxe ("en trop à la fin : " ^ (print_token t)))

let lit_fichier (nom : string) : string =
  let ic : in_channel = open_in nom in
  let s : string = really_input_string ic (in_channel_length ic) in
  close_in ic; s

(* lit un programme écrit en texte, puis le type et l'évalue *)
let execute (s : string) : unit =
  try
    let t : pterm = lit_programme s in
    print_endline (inference t);
    evaluation t 1000
  with Erreur_syntaxe m -> print_endline ("***ERREUR DE SYNTAXE*** " ^ m)
                         
(* ***EXEMPLES*** *)  
let ex_id : pterm = Abs ("x", Var "x") 
let inf_ex_id : string = inference ex_id 
let ex_k : pterm = Abs ("x", Abs ("y", Var "x")) 
let inf_ex_k : string = inference ex_k
let ex_s : pterm = Abs ("x", Abs ("y", Abs ("z", App (App (Var "x", Var "z"), App (Var "y", Var "z")))))
let inf_ex_s : string = inference ex_s 
let ex_nat1 : pterm = App (Abs ("x", Add(Var "x", N 1)), N 3)
let inf_ex_nat1 : string = inference ex_nat1
let ex_nat2 : pterm = Abs ("x", Add( Var "x", Var "x"))
let inf_ex_nat2 : string = inference ex_nat2
let ex_omega : pterm = App (Abs ("x", App (Var "x", Var "x")), Abs ("y", App (Var "y", Var "y")))
let inf_ex_omega : string = inference ex_omega
let ex_nat3 : pterm = App (ex_nat2, ex_id)
let inf_ex_nat3 : string = inference ex_nat3

(* ***EXEMPLES EVALUATION*** *)
let ex_ii : pterm = App (ex_id, ex_id)
let ex_kii : pterm = App (App (ex_k, ex_id), ex_id)
let ex_skk3 : pterm = App (App (App (ex_s, ex_k), ex_k), N 3)
(* l'argument 2 + 3 est calculé avant l'appel *)
let ex_add : pterm = App (Abs ("x", Add (Var "x", N 1)), Add (N 2, N 3))
(* y est libre : il ne doit pas etre capturé par le fun y *)
let ex_capture : pterm = App (Abs ("x", Abs ("y", Var "x")), Var "y")
(* K I Omega : en call-by-value on calcule Omega meme si K ne s'en sert pas *)
let ex_kiomega : pterm = App (App (ex_k, ex_id), ex_omega)

(* exemples entiers, listes, let *)
let ex_sub : pterm = Sub (N 10, Add (N 3, N 4))
let inf_ex_sub : string = inference ex_sub
let ex_liste : pterm = Cons (N 1, Cons (N 2, Cons (N 3, Nil)))
let inf_ex_liste : string = inference ex_liste
let ex_hd : pterm = Hd (Tl ex_liste)
let inf_ex_hd : string = inference ex_hd
let ex_ifz : pterm = IfZero (Sub (N 2, N 2), N 10, N 20)
let inf_ex_ifz : string = inference ex_ifz
let ex_ife : pterm = IfEmpty (Tl (Cons (N 1, Nil)), N 1, N 0)
let inf_ex_ife : string = inference ex_ife
let ex_let : pterm = Let ("x", Add (N 1, N 2), Add (Var "x", Var "x"))
let inf_ex_let : string = inference ex_let
(* exemple du cours : let i = fun x -> x in i i 2 *)
let ex_poly : pterm = Let ("i", Abs ("x", Var "x"), App (App (Var "i", Var "i"), N 2))
let inf_ex_poly : string = inference ex_poly
(* pareil avec un fun au lieu du let : pas typable *)
let ex_poly2 : pterm = App (Abs ("i", App (App (Var "i", Var "i"), N 2)), Abs ("x", Var "x"))
let inf_ex_poly2 : string = inference ex_poly2
(* somme de 0 à n *)
let ex_somme : pterm = Fix (Abs ("f", Abs ("n", IfZero (Var "n", N 0, Add (Var "n", App (Var "f", Sub (Var "n", N 1)))))))
let inf_ex_somme : string = inference ex_somme
let ex_somme3 : pterm = App (ex_somme, N 3)
(* longueur d'une liste *)
let ex_longueur : pterm = Fix (Abs ("lg", Abs ("l", IfEmpty (Var "l", N 0, Add (N 1, App (Var "lg", Tl (Var "l")))))))
let inf_ex_longueur : string = inference ex_longueur
let ex_longueur3 : pterm = App (ex_longueur, ex_liste)
let ex_err : pterm = Add (N 1, Nil)
let inf_ex_err : string = inference ex_err
(* le let garde la contrainte y : Nat, donc l'appel avec [] est refusé *)
let ex_let_contrainte : pterm = Abs ("y", Let ("x", Add (Var "y", N 1), Var "y"))
let inf_ex_let_contrainte : string = inference ex_let_contrainte
let ex_let_contrainte2 : pterm = App (ex_let_contrainte, Nil)
let inf_ex_let_contrainte2 : string = inference ex_let_contrainte2
(* hd [] est typable mais bloque *)
let ex_hd_vide : pterm = Hd Nil
let inf_ex_hd_vide : string = inference ex_hd_vide

(* exemples traits impératifs *)
let ex_ref : pterm = Let ("r", Ref (N 1), Let ("_", Assign (Var "r", Add (Deref (Var "r"), N 41)), Deref (Var "r")))
let inf_ex_ref : string = inference ex_ref
let ex_compteur : pterm = Let ("c", Ref (N 0), Let ("incr", Abs ("u", Assign (Var "c", Add (Deref (Var "c"), N 1))),
  Let ("_", App (Var "incr", Unit), Let ("_", App (Var "incr", Unit), Deref (Var "c")))))
let inf_ex_compteur : string = inference ex_compteur
(* exemple du sujet : doit etre refusé grace au polymorphisme faible *)
let ex_faible : pterm = Let ("l", Ref Nil,
  Let ("_", Assign (Var "l", Cons (Abs ("x", Var "x"), Nil)), Add (Hd (Deref (Var "l")), N 2)))
let inf_ex_faible : string = inference ex_faible
(* pareil avec un entier dans la liste : ok *)
let ex_faible2 : pterm = Let ("l", Ref Nil,
  Let ("_", Assign (Var "l", Cons (N 1, Nil)), Add (Hd (Deref (Var "l")), N 2)))
let inf_ex_faible2 : string = inference ex_faible2
(* exemple du cours : let f = ref (fun x -> x) in (fun y -> !f ()) (f := fun z -> z + 1) *)
let ex_cours_ref : pterm = Let ("f", Ref (Abs ("x", Var "x")),
  App (Abs ("y", App (Deref (Var "f"), Unit)), Assign (Var "f", Abs ("z", Add (Var "z", N 1)))))
let inf_ex_cours_ref : string = inference ex_cours_ref
(* mkref [] est une application donc expansive : refusé aussi *)
let ex_mkref : pterm = Let ("mkref", Abs ("x", Ref (Var "x")), Let ("l", App (Var "mkref", Nil),
  Let ("_", Assign (Var "l", Cons (Abs ("x", Var "x"), Nil)), Add (Hd (Deref (Var "l")), N 2))))
let inf_ex_mkref : string = inference ex_mkref
(* fun x -> ref x est non expansif donc reste polymorphe *)
let ex_mkref2 : pterm = Let ("mkref", Abs ("x", Ref (Var "x")),
  Let ("a", App (Var "mkref", N 1), Let ("b", App (Var "mkref", Nil), Deref (Var "a"))))
let inf_ex_mkref2 : string = inference ex_mkref2

(* exemples records et option *)
let ex_point : pterm = Record [("x", N 1); ("y", Add (N 1, N 1))]
let inf_ex_point : string = inference ex_point
let ex_champ : pterm = Champ (ex_point, "y")
let inf_ex_champ : string = inference ex_champ
(* polymorphisme de rangée : getx marche pour tous les records qui ont un champ x *)
let ex_getx : pterm = Abs ("p", Champ (Var "p", "x"))
let inf_ex_getx : string = inference ex_getx
let ex_rangee : pterm = Let ("getx", ex_getx,
  Add (App (Var "getx", ex_point), App (Var "getx", Record [("x", N 10); ("z", Nil)])))
let inf_ex_rangee : string = inference ex_rangee
let ex_champ_err : pterm = Champ (Record [("x", N 1)], "y")
let inf_ex_champ_err : string = inference ex_champ_err
let ex_opt : pterm = MatchOpt (OptSome (N 5), N 0, "v", Add (Var "v", N 1))
let inf_ex_opt : string = inference ex_opt
(* hd qui renvoie une option au lieu de bloquer *)
let ex_hdopt : pterm = Abs ("l", IfEmpty (Var "l", OptNone, OptSome (Hd (Var "l"))))
let inf_ex_hdopt : string = inference ex_hdopt
let ex_hdopt2 : pterm = Let ("hd_opt", ex_hdopt, MatchOpt (App (Var "hd_opt", Nil), N 0, "v", Var "v"))
let inf_ex_hdopt2 : string = inference ex_hdopt2
let ex_opt_err : pterm = MatchOpt (N 3, N 0, "v", Var "v")
let inf_ex_opt_err : string = inference ex_opt_err

(* exemples exceptions *)
let ex_exn : pterm = Try (Add (N 1, Raise (N 42)), "e", Var "e")
let inf_ex_exn : string = inference ex_exn
let ex_tete : pterm = Abs ("l", IfEmpty (Var "l", Raise (N 0), Hd (Var "l")))
let inf_ex_tete : string = inference ex_tete
let ex_tete2 : pterm = Let ("tete", ex_tete, Try (App (Var "tete", Nil), "e", N 99))
let inf_ex_tete2 : string = inference ex_tete2
(* le raise dans f est rattrapé par le try où f est appelée *)
let ex_exn_f : pterm = Let ("f", Abs ("x", IfZero (Var "x", Raise (N 5), Var "x")),
  Try (Add (App (Var "f", N 3), App (Var "f", N 0)), "e", Var "e"))
let inf_ex_exn_f : string = inference ex_exn_f
(* on relance une exception dans le handler *)
let ex_relance : pterm = Try (Try (Raise (N 1), "x", Raise (Add (Var "x", N 1))), "y", Add (Var "y", N 10))
let inf_ex_relance : string = inference ex_relance
let ex_non_rattrapee : pterm = Add (N 1, Raise (N 7))
let inf_ex_non_rattrapee : string = inference ex_non_rattrapee
(* une exception n'annule pas les affectations déjà faites *)
let ex_exn_ref : pterm = Let ("r", Ref (N 0),
  Let ("_", Try (Let ("_", Assign (Var "r", N 1), Raise (N 2)), "e", Unit), Deref (Var "r")))
let inf_ex_exn_ref : string = inference ex_exn_ref
let ex_exn_err : pterm = Raise Nil
let inf_ex_exn_err : string = inference ex_exn_err

(* exemples objets *)
let classe_point (corps : pterm) : pterm = Classe ("Point", None, [("x", Nat); ("y", Nat)],
  [("plus", "p", TClasse "Point", TClasse "Point",
      New ("Point", [("x", Add (Attribut (Var "self", "x"), Attribut (Var "p", "x")));
                     ("y", Add (Attribut (Var "self", "y"), Attribut (Var "p", "y")))]));
   ("somme", "u", TUnit, Nat, Add (Attribut (Var "self", "x"), Attribut (Var "self", "y")))], corps)
(* PointC hérite de Point, ajoute une couleur c et redéfinit somme *)
let classe_pointc (corps : pterm) : pterm = Classe ("PointC", Some "Point", [("c", Nat)],
  [("somme", "u", TUnit, Nat, Add (Add (Attribut (Var "self", "x"), Attribut (Var "self", "y")), Attribut (Var "self", "c")))], corps)
let ex_objet : pterm = classe_point (Let ("a", New ("Point", [("x", N 1); ("y", N 2)]),
  Let ("b", New ("Point", [("x", N 10); ("y", N 20)]), Appel (Appel (Var "a", "plus", Var "b"), "somme", Unit))))
let inf_ex_objet : string = inference ex_objet
(* a#plus(b) avec b : PointC (sous-typage), b#somme(()) appelle la version de PointC *)
let ex_heritage : pterm = classe_point (classe_pointc (Let ("a", New ("Point", [("x", N 1); ("y", N 2)]),
  Let ("b", New ("PointC", [("x", N 10); ("y", N 20); ("c", N 100)]),
    Add (Appel (Appel (Var "a", "plus", Var "b"), "somme", Unit), Appel (Var "b", "somme", Unit))))))
let inf_ex_heritage : string = inference ex_heritage
(* une fonction qui prend un Point marche aussi avec un PointC *)
let ex_absT : pterm = classe_point (classe_pointc (Let ("norme", AbsT ("p", TClasse "Point", Appel (Var "p", "somme", Unit)),
  App (Var "norme", New ("PointC", [("x", N 1); ("y", N 2); ("c", N 3)])))))
let inf_ex_absT : string = inference ex_absT
(* dans l'autre sens c'est refusé *)
let ex_sous_type_err : pterm = classe_point (classe_pointc (Let ("f", AbsT ("p", TClasse "PointC", Attribut (Var "p", "c")),
  App (Var "f", New ("Point", [("x", N 1); ("y", N 2)])))))
let inf_ex_sous_type_err : string = inference ex_sous_type_err
(* sans le :> la liste est refusée car le premier élément fixe le type PointC *)
let ex_coerce : pterm = classe_point (classe_pointc (Cons (Coerce (New ("PointC", [("x", N 1); ("y", N 2); ("c", N 3)]), TClasse "Point"),
  Cons (New ("Point", [("x", N 4); ("y", N 5)]), Nil))))
let inf_ex_coerce : string = inference ex_coerce
let ex_coerce_err : pterm = classe_point (classe_pointc (Cons (New ("PointC", [("x", N 1); ("y", N 2); ("c", N 3)]),
  Cons (New ("Point", [("x", N 4); ("y", N 5)]), Nil))))
let inf_ex_coerce_err : string = inference ex_coerce_err
(* surcharge (cf cours 06) : PE a dist(P) (héritée) et dist(PE) *)
let classes_p_pe (corps : pterm) : pterm = Classe ("P", None, [("a", Nat)],
    [("dist", "p", TClasse "P", Nat, Sub (Attribut (Var "self", "a"), Attribut (Var "p", "a")))],
  Classe ("PE", Some "P", [("b", Nat)],
    [("dist", "p", TClasse "PE", Nat, Add (Sub (Attribut (Var "self", "a"), Attribut (Var "p", "a")),
                                           Sub (Attribut (Var "self", "b"), Attribut (Var "p", "b"))))], corps))
let ex_surcharge : pterm = classes_p_pe (Let ("o", New ("PE", [("a", N 10); ("b", N 10)]), Let ("p1", New ("P", [("a", N 1)]),
    Let ("p2", New ("PE", [("a", N 1); ("b", N 1)]),
      Record [("avec_p", Appel (Var "o", "dist", Var "p1")); ("avec_pe", Appel (Var "o", "dist", Var "p2"))]))))
let inf_ex_surcharge : string = inference ex_surcharge
(* après un upcast en P, le typage choisit dist(P) mais l'exécution prend dist(PE) : 18 (Java donnerait 9) *)
let ex_surcharge_upcast : pterm = classes_p_pe (Let ("o", New ("PE", [("a", N 10); ("b", N 10)]),
  Let ("q", Coerce (New ("PE", [("a", N 1); ("b", N 1)]), TClasse "P"), Appel (Var "o", "dist", Var "q"))))
let inf_ex_surcharge_upcast : string = inference ex_surcharge_upcast
let ex_surcharge_err : pterm = Classe ("A", None, [], [("f", "x", Nat, Nat, Var "x"); ("f", "x", TUnit, Nat, N 0)], N 0)
let inf_ex_surcharge_err : string = inference ex_surcharge_err
(* redéfinition avec un type de retour incompatible *)
let ex_redef_err : pterm = Classe ("A", None, [], [("m", "x", Nat, Nat, Var "x")],
  Classe ("B", Some "A", [], [("m", "x", Nat, TUnit, Unit)], N 0))
let inf_ex_redef_err : string = inference ex_redef_err
(* Ref est invariant : sinon on pourrait mettre un Point dans une ref de PointC puis lire son champ c *)
let ex_ref_invariant : pterm = classe_point (classe_pointc (Let ("r", Ref (New ("PointC", [("x", N 1); ("y", N 2); ("c", N 3)])),
  Let ("f", AbsT ("q", TRef (TClasse "Point"), Assign (Var "q", New ("Point", [("x", N 4); ("y", N 5)]))),
    Let ("_", App (Var "f", Var "r"), Attribut (Deref (Var "r"), "c"))))))
let inf_ex_ref_invariant : string = inference ex_ref_invariant

(* exemples écrits en texte, lus par le parser *)
let programmes : string list = [
  "let id = fun x -> x in id id 2";
  "let rec somme n = if zero n then 0 else n + somme (n - 1) in somme 3";
  "let l = ref [] in let _ = l := [fun x -> x] in hd !l + 2";
  "(* polymorphisme de rangée *) let getx p = p.x in getx {x = 1; y = 2} + getx {x = 10; z = []}";
  "let tete l = if empty l then raise 0 else hd l in try tete [] with e -> 99";
  "class Point = {x : Nat; y : Nat; somme (u : unit) : Nat = self#x + self#y} in (new Point {x = 1; y = 2})#somme()";
  "let x = in 3" ]

(* tous les exemples, pour vérifier que le parser relit ce que print_term écrit *)
let tous_les_exemples : pterm list = [
  ex_id; ex_k; ex_s; ex_nat1; ex_nat2; ex_omega; ex_nat3; ex_ii;
  ex_kii; ex_skk3; ex_add; ex_capture; ex_kiomega; ex_sub; ex_liste; ex_hd;
  ex_ifz; ex_ife; ex_let; ex_poly; ex_poly2; ex_somme; ex_somme3; ex_longueur;
  ex_longueur3; ex_err; ex_let_contrainte; ex_let_contrainte2; ex_hd_vide; ex_ref; ex_compteur; ex_faible;
  ex_faible2; ex_cours_ref; ex_mkref; ex_mkref2; ex_point; ex_champ; ex_getx; ex_rangee;
  ex_champ_err; ex_opt; ex_hdopt; ex_hdopt2; ex_opt_err; ex_exn; ex_tete; ex_tete2;
  ex_exn_f; ex_relance; ex_non_rattrapee; ex_exn_ref; ex_exn_err; ex_objet; ex_heritage; ex_absT;
  ex_sous_type_err; ex_coerce; ex_coerce_err; ex_surcharge; ex_surcharge_upcast; ex_surcharge_err; ex_redef_err; ex_ref_invariant ]

let relecture (ts : pterm list) : string =
  let rates : pterm list = List.filter (fun t -> try lit_programme (print_term t) <> t with Erreur_syntaxe _ -> true) ts in
  if rates = [] then "relecture de " ^ (string_of_int (List.length ts)) ^ " exemples par le parser : OK"
  else "relecture ratee pour " ^ (string_of_int (List.length rates)) ^ " exemples, par exemple " ^ (print_term (List.hd rates))

let main () =
 print_endline "======================";
 print_endline inf_ex_id;
 print_endline "======================";
 print_endline inf_ex_k;
 print_endline "======================";
 print_endline inf_ex_s;
 print_endline "======================";
 print_endline inf_ex_omega;
 print_endline "======================";
 print_endline inf_ex_nat1;
 print_endline "======================";
 print_endline inf_ex_nat2;
 print_endline "======================";
 print_endline inf_ex_nat3;
 print_endline "======================";
 print_endline inf_ex_sub;
 print_endline "======================";
 print_endline inf_ex_liste;
 print_endline "======================";
 print_endline inf_ex_hd;
 print_endline "======================";
 print_endline inf_ex_ifz;
 print_endline "======================";
 print_endline inf_ex_ife;
 print_endline "======================";
 print_endline inf_ex_let;
 print_endline "======================";
 print_endline inf_ex_poly;
 print_endline "======================";
 print_endline inf_ex_poly2;
 print_endline "======================";
 print_endline inf_ex_somme;
 print_endline "======================";
 print_endline inf_ex_longueur;
 print_endline "======================";
 print_endline inf_ex_err;
 print_endline "======================";
 print_endline inf_ex_let_contrainte;
 print_endline "======================";
 print_endline inf_ex_let_contrainte2;
 print_endline "======================";
 print_endline inf_ex_hd_vide;
 print_endline "======================";
 print_endline inf_ex_ref;
 print_endline "======================";
 print_endline inf_ex_compteur;
 print_endline "======================";
 print_endline inf_ex_faible;
 print_endline "======================";
 print_endline inf_ex_faible2;
 print_endline "======================";
 print_endline inf_ex_cours_ref;
 print_endline "======================";
 print_endline inf_ex_mkref;
 print_endline "======================";
 print_endline inf_ex_mkref2;
 print_endline "======================";
 print_endline inf_ex_point;
 print_endline "======================";
 print_endline inf_ex_champ;
 print_endline "======================";
 print_endline inf_ex_getx;
 print_endline "======================";
 print_endline inf_ex_rangee;
 print_endline "======================";
 print_endline inf_ex_champ_err;
 print_endline "======================";
 print_endline inf_ex_opt;
 print_endline "======================";
 print_endline inf_ex_hdopt;
 print_endline "======================";
 print_endline inf_ex_hdopt2;
 print_endline "======================";
 print_endline inf_ex_opt_err;
 print_endline "======================";
 print_endline inf_ex_exn;
 print_endline "======================";
 print_endline inf_ex_tete;
 print_endline "======================";
 print_endline inf_ex_tete2;
 print_endline "======================";
 print_endline inf_ex_exn_f;
 print_endline "======================";
 print_endline inf_ex_relance;
 print_endline "======================";
 print_endline inf_ex_non_rattrapee;
 print_endline "======================";
 print_endline inf_ex_exn_ref;
 print_endline "======================";
 print_endline inf_ex_exn_err;
 print_endline "======================";
 print_endline inf_ex_objet;
 print_endline "======================";
 print_endline inf_ex_heritage;
 print_endline "======================";
 print_endline inf_ex_absT;
 print_endline "======================";
 print_endline inf_ex_sous_type_err;
 print_endline "======================";
 print_endline inf_ex_coerce;
 print_endline "======================";
 print_endline inf_ex_coerce_err;
 print_endline "======================";
 print_endline inf_ex_surcharge;
 print_endline "======================";
 print_endline inf_ex_surcharge_err;
 print_endline "======================";
 print_endline inf_ex_redef_err;
 print_endline "======================";
 print_endline inf_ex_ref_invariant;
 print_endline "======================";
 print_endline inf_ex_surcharge_upcast;
 print_endline "";
 print_endline "*** EVALUATION ***";
 print_endline "======================";
 evaluation ex_nat1 100;
 print_endline "======================";
 evaluation ex_add 100;
 print_endline "======================";
 evaluation ex_ii 100;
 print_endline "======================";
 evaluation ex_kii 100;
 print_endline "======================";
 evaluation ex_skk3 100;
 print_endline "======================";
 evaluation ex_capture 100;
 print_endline "======================";
 evaluation ex_nat3 100;
 print_endline "======================";
 evaluation ex_omega 10;
 print_endline "======================";
 evaluation ex_kiomega 10;
 print_endline "======================";
 evaluation ex_sub 100;
 print_endline "======================";
 evaluation ex_hd 100;
 print_endline "======================";
 evaluation ex_ifz 100;
 print_endline "======================";
 evaluation ex_ife 100;
 print_endline "======================";
 evaluation ex_let 100;
 print_endline "======================";
 evaluation ex_poly 100;
 print_endline "======================";
 evaluation ex_somme3 100;
 print_endline "======================";
 evaluation ex_longueur3 100;
 print_endline "======================";
 evaluation ex_err 100;
 print_endline "======================";
 evaluation ex_let_contrainte2 100;
 print_endline "======================";
 evaluation ex_hd_vide 100;
 print_endline "======================";
 evaluation ex_ref 100;
 print_endline "======================";
 evaluation ex_compteur 100;
 print_endline "======================";
 evaluation ex_faible 100;
 print_endline "======================";
 evaluation ex_faible2 100;
 print_endline "======================";
 evaluation ex_cours_ref 100;
 print_endline "======================";
 evaluation ex_champ 100;
 print_endline "======================";
 evaluation ex_rangee 100;
 print_endline "======================";
 evaluation ex_champ_err 100;
 print_endline "======================";
 evaluation ex_opt 100;
 print_endline "======================";
 evaluation ex_hdopt2 100;
 print_endline "======================";
 evaluation ex_exn 100;
 print_endline "======================";
 evaluation ex_tete2 100;
 print_endline "======================";
 evaluation ex_exn_f 100;
 print_endline "======================";
 evaluation ex_relance 100;
 print_endline "======================";
 evaluation ex_non_rattrapee 100;
 print_endline "======================";
 evaluation ex_exn_ref 100;
 print_endline "======================";
 evaluation ex_objet 100;
 print_endline "======================";
 evaluation ex_heritage 100;
 print_endline "======================";
 evaluation ex_absT 100;
 print_endline "======================";
 evaluation ex_surcharge 100;
 print_endline "======================";
 evaluation ex_ref_invariant 100;
 print_endline "======================";
 evaluation ex_surcharge_upcast 100;
 print_endline "";
 print_endline "*** PARSER ***";
 List.iter (fun s -> print_endline "======================"; print_endline ("texte : " ^ s); execute s) programmes;
 print_endline "======================";
 print_endline (relecture tous_les_exemples)

(* ocaml minitypeur.ml : les exemples, ocaml minitypeur.ml fichier : le programme du fichier (rien si chargé par #use) *)
let _ =
  if Filename.basename Sys.argv.(0) = "minitypeur.ml" then
    (if Array.length Sys.argv > 1 then
       (try execute (lit_fichier Sys.argv.(1)) with Sys_error m -> print_endline m)
     else main ())