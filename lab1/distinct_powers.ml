open Z

let powZ (a : int) (b : int) : Z.t = Z.pow (Z.of_int a) b

(* 1.1 Нехвостовая рекурсия*)
let distinct_rec amin amax bmin bmax : int =
  let rec for_a a =
    if a > amax then []
    else
      let rec for_b b = if b > bmax then [] else powZ a b :: for_b (b + 1) in
      let row = for_b bmin in
      row @ for_a (a + 1)
  in
  let xs = for_a amin in
  let ys = List.sort_uniq Z.compare xs in
  List.length ys

(* 1.2 Хвостовая рекурсия*)
let distinct_tail amin amax bmin bmax : int =
  let rec for_a a acc =
    if a > amax then List.length (List.sort_uniq Z.compare acc)
    else
      let rec for_b b acc' =
        if b > bmax then acc' else for_b (b + 1) (powZ a b :: acc')
      in
      for_a (a + 1) (for_b bmin acc)
  in
  for_a amin []

(* 2 Модульная реализация*)
module ZOrd = struct
  type t = Z.t

  let compare = Z.compare
end

module ZSet = Set.Make (ZOrd)

let gen_pairs amin amax bmin bmax : (int * int) list =
  let rec go_a a acc =
    if a < amin then acc
    else
      let rec go_b b acc' =
        if b < bmin then acc' else go_b (b - 1) ((a, b) :: acc')
      in
      go_a (a - 1) (go_b bmax acc)
  in
  go_a amax []

let pow_of_pair (a, b) = powZ a b

let dedup_len (xs : Z.t list) : int =
  List.fold_left (fun s x -> ZSet.add x s) ZSet.empty xs |> ZSet.cardinal

let distinct_modular amin amax bmin bmax : int =
  gen_pairs amin amax bmin bmax |> List.map pow_of_pair |> dedup_len

(* 3 Генерация последовательности с map*)
let distinct_map amin amax bmin bmax : int =
  let na = amax - amin + 1 in
  let nb = bmax - bmin + 1 in
  let n = na * nb in
  List.init n (fun i ->
      let da = i / nb and db = i mod nb in
      (amin + da, bmin + db))
  |> List.map pow_of_pair |> List.sort_uniq Z.compare |> List.length

(* 4 Спец синтаксис для циклов *)
let distinct_for amin amax bmin bmax : int =
  let h = Hashtbl.create 10000 in
  for a = amin to amax do
    for b = bmin to bmax do
      let k = Z.to_string (powZ a b) in
      Hashtbl.replace h k ()
    done
  done;
  Hashtbl.length h


(* 5 Работа с бесконечными списками для языков, поддерживающих ленивые коллекции или итераторы как часть языка*)
let seq_pairs amin amax bmin bmax : (int * int) Seq.t =
  let rec next a b () =
    if a > amax then Seq.Nil
    else if b > bmax then Seq.Cons ((a, bmin), next (a + 1) bmin)
    else Seq.Cons ((a, b), next a (b + 1))
  in
  next amin bmin

let distinct_seq amin amax bmin bmax : int =
  seq_pairs amin amax bmin bmax
  |> Seq.map (fun (a, b) -> powZ a b)
  |> Seq.fold_left (fun s x -> ZSet.add x s) ZSet.empty
  |> ZSet.cardinal

let () =
  let amin, amax, bmin, bmax = (2, 100, 2, 100) in
  let results =
    [
      ("1.1 Рекурсия", distinct_rec amin amax bmin bmax);
      ("1.2 Хвостовая рекурсия", distinct_tail amin amax bmin bmax);
      ("2 Модульная", distinct_modular amin amax bmin bmax);
      ("3 Map-генерация + fold", distinct_map amin amax bmin bmax);
      ("4 Цикл for по массиву", distinct_for amin amax bmin bmax);
      ("5 Seq (бесконечный поток)", distinct_seq amin amax bmin bmax);
    ]
  in

  List.iter
    (fun (name, value) -> Printf.printf "%-30s: %d\n" name value)
    results
