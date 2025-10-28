# Лабораторная работа 1

## Сидимеков Дмитрий Алексеевич P3322

### Вариант 2

Найти сумму чётных чисел Фибоначчи, не превышающих заданный предел (в задаче - 4 000 000).

Исходная последовательность:
1, 2, 3, 5, 8, 13, 21, 34, 55, ...
Нужно отобрать чётные элементы и просуммировать их.

Для каждой проблемы должно быть представлено несколько решений:

- рекурсия и хвостовая рекурсия,
- модульная реализация через generate, filter, fold,
- генерация с использованием `map`,
- работа со спец синтаксисом для циклов,
- работа с бесконечными ленивыми коллекциями

Для сравнения добавлено решение на Python.

#### 1. Рекурсия и хвостовая рекурсия

```ocaml
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
```

Во втором варианте рекурсивный вызов - это последнее действие, стек не растёт.

---

#### 2. Модульная реализация

```ocaml
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
```

Поток данных: генерация списка, фильтрация, свёртка.

---

#### 3. Генерация последовательности при помощи `map`

```ocaml
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

(* свертка до суммы*)
let fold_sum = List.fold_left ( + ) 0

(* конвейер со сверткой*)
let even_fib_sum_map_gen limit = gen_even_fib_map limit |> fold_sum
```

Чётные числа фибоначчи находятся на индексах `3i + 2` (по нумерации из условия задачи)
Используется `List.map`: индексы `[2;5;8;...]` - соответствующие числа Фибоначчи - сумма.

---

#### 4. Спец. синтаксис циклов

```ocaml
(* 4 Работа со спец. синтаксисом для циклов *)
(* свёртка циклом по массиву*)
let even_fib_sum_for (limit : int) : int =
  let xs = Array.of_list (fib_upto limit) in
  let s = ref 0 in
  for i = 0 to Array.length xs - 1 do
    if xs.(i) mod 2 = 0 then s := !s + xs.(i)
  done;
  !s
```

Реализация со спец синтаксисом цикла `for`

---

#### 5. Бесконечные последовательности

```ocaml
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
```

`Seq` — ленивый итератор. Элементы вычисляются по требованию, поток потребляется один раз.

---

#### Сравнение с императивным решением (Python)

```python
def even_fib_sum_iter(limit = 4 * 10**6):
    a, b = 1, 2
    s = 0
    while a <= limit:
        if a % 2 == 0:
            s += a
        a, b = b, a + b

    return s
```

Реализует ту же логику, но через явный цикл и изменяемые переменные.

### Вариант 29

Рассмотриваются все целочисленные комбинации $a^b$ при $2 \le a \le 5$ и $2 \le b \le 5$:

$$
2^2=4,\; 2^3=8,\; 2^4=16,\; 2^5=32 \\
3^2=9,\; 3^3=27,\; 3^4=81,\; 3^5=243 \\
4^2=16,\; 4^3=64,\; 4^4=256,\; 4^5=1024 \\
5^2=25,\; 5^3=125,\; 5^4=625,\; 5^5=3125 \\
$$

Если упорядочить эти значения по возрастанию и убрать повторы, получаются 15 различных членов
$4, 8, 9, 16, 25, 27, 32, 64, 81, 125, 243, 256, 625, 1024, 3125$

Сколько различных членов будет в последовательности, образованной числами $a^b$ при $2 \le a \le 100$ и $2 \le b \le 100$?

#### 1. Рекурсия и хвостовая рекурсия

```ocaml
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
```

---

#### 2. Модульная реализация

```ocaml
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
```

---

#### 3. Генерация последовательности при помощи `map`

```ocaml
(* 3 Генерация последовательности с map*)
let distinct_map amin amax bmin bmax : int =
  let na = amax - amin + 1 in
  let nb = bmax - bmin + 1 in
  let n = na * nb in
  List.init n (fun i ->
      let da = i / nb and db = i mod nb in
      (amin + da, bmin + db))
  |> List.map pow_of_pair |> List.sort_uniq Z.compare |> List.length
```

---

#### 4. Спец. синтаксис циклов

```ocaml
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
```

Реализация со спец синтаксисом цикла `for`

---

#### 5. Бесконечные последовательности

```ocaml
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
```

---

#### Сравнение с императивным решением (Python)

```python
def distinct_powers_py(amin=2, amax=100, bmin=2, bmax=100):
    return len({pow(a,b) for a in range(amin, amax+1) for b in range(bmin, bmax+1)})

print(distinct_powers_py())  # 9183
```

---

### Выводы

1. **Рекурсия** - базовый способ описания итерации в функциональном стиле.
   Хвостовая рекурсия устраняет рост стека и эквивалентна циклу.

2. **Композиция функций** (`|>`, `filter`, `fold_left`) - приём для потока данных. Данные проходят через фильтр и свёртку в потоке.

3. **Отображение `map`** используется для генерации значений по известным индексам.
   Мы заранее формируем список индексов чётных членов последовательности и преобразуем его функцией `fib_n`.
   Приём показывает идею функции как объекта: операция вычисления Фибоначчи передаётся как аргумент в map.

4. **Циклы** показывают, как ту же задачу можно решить императивно.
   В OCaml такие конструкции есть, но они занимают вспомогательную роль, основной акцент языка на чистых функциях и композиции.

5. **Ленивые структуры (`Seq.t`)** дают возможность описывать бесконечные потоки без переполнения памяти.

   - `Seq` — итератор: значения вычисляются по запросу, но не сохраняются.
   - Ленивые коллекции (`Lazy.t`) — мемоизируют вычисления, повторное использование.
