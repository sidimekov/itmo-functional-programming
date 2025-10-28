(* 1.1 Рекурсия*)
let even_fib_sum_rec (limit : int) : int =
  let rec fib a b =
    if a > limit then 0
    else
      let add = if a mod 2 = 0 then a else 0 in
      add + fib b (a + b)
  in
  fib 1 2

(* 1.2 Хвостовая рекурсия*)
let even_fib_sum_tail (limit : int) : int =
  let rec fib a b sum =
    if a > limit then sum
    else
      let sum1 = if a mod 2 = 0 then sum + a else sum in
      fib b (a + b) sum1
  in
  fib 1 2 0

(* 2 Модульная реализация*)
(* генерация чисел фибоначчи до limit через хвост*)
let fib_upto (limit : int) : int list =
  let rec loop a b acc =
    if a > limit then List.rev acc
    else
      let acc1 = a :: acc in
      loop b (a + b) acc1
  in
  loop 1 2 []

(* фильтрация по чётным*)
let is_even x = x mod 2 = 0

(* свертка до суммы*)
let fold_sum = List.fold_left ( + ) 0

(* конвейер*)
let even_fib_sum_module (limit : int) : int =
  fib_upto limit |> List.filter is_even |> fold_sum

(* 3 Генерация последовательности при помощи отображения map*)
(* n-е число фибоначчи*)
let fib_n (n : int) : int =
  let rec loop i a b = if i = 1 then a else loop (i - 1) b (a + b) in
  loop n 1 2

(* подсчёт количества чётных чисел фибоначчи <= limit для генерации*)
let count_fib_even_le (limit : int) : int =
  let rec loop a b k =
    if a > limit then k
    else
      let k1 = if a mod 2 = 0 then k + 1 else k in
      loop b (a + b) k1
  in
  loop 1 2 0

(* генерация списка индексов
чётные фибоначчи находятся на индексах 3i + 2*)
let gen_even_fib_indices limit =
  let n = count_fib_even_le limit in
  List.init n (fun i -> (3 * i) + 2)

(* применение map*)
let gen_even_fib_map limit = gen_even_fib_indices limit |> List.map fib_n

(* конвейер со сверткой*)
let even_fib_sum_map_gen limit = gen_even_fib_map limit |> fold_sum

(* 4 Работа со спец. синтаксисом для циклов *)
(* свёртка циклом по массиву*)
let even_fib_sum_for (limit : int) : int =
  let xs = Array.of_list (fib_upto limit) in
  let s = ref 0 in
  for i = 0 to Array.length xs - 1 do
    if xs.(i) mod 2 = 0 then s := !s + xs.(i)
  done;
  !s

(* 5 Работа с бесконечными списками для языков, поддерживающих ленивые коллекции или итераторы как часть языка*)
let rec fib_seq_from a b () = Seq.Cons (a, fib_seq_from b (a + b))
let fib_seq : int Seq.t = fib_seq_from 1 2

(* свертка Seq*)
let sum_seq = Seq.fold_left ( + ) 0

(* сумма чётных фибоначчи <= limit*)
let even_fib_sum_seq (limit : int) : int =
  fib_seq
  |> Seq.take_while (fun x -> x <= limit)
  |> Seq.filter is_even |> sum_seq

let () =
  let limit = 4_000_000 in
  let results = [
    ("1.1 Рекурсия", even_fib_sum_rec limit);
    ("1.2 Хвостовая рекурсия", even_fib_sum_tail limit);
    ("2 Модульная", even_fib_sum_module limit);
    ("3 Map-генерация + fold", even_fib_sum_map_gen limit);
    ("4 Цикл for по массиву", even_fib_sum_for limit);
    ("5 Seq (бесконечный поток)", even_fib_sum_seq limit);
  ] in

  List.iter (fun (name, value) ->
    Printf.printf "%-28s: %d\n" name value
  ) results;
