-- первый для всех гхц кроме 7.10, новых ghcjs, клина. Второй для ghc 7.10, старых ghcjs, eta, haste

primes = 2:3:filter isPrime [5,7..] :: [Int]
isPrime x = all (/= 0) . map (rem x) . takeWhile ((<= x) . (^2)) $ primes
main = print . length . takeWhile (<= 2^24) $ primes

primes = 2:3:filter isPrime [5,7..] :: [Int]
isPrime x = foldr (&&) True . map (/= 0) . map (rem x) . takeWhile ((<= x) . (^2)) $ primes
main = print . length . takeWhile (<= 2^24) $ primes

-- Ocalm

open Batteries

let rec primes = lazy(LazyList.(2^:^3^:^filter is_prime (seq 5 ((+) 2) (fun _ -> true))))
and is_prime x = LazyList.(for_all ((<>) 0) % map ((mod) x) % take_while (fun y -> y*y <= x) @@ Lazy.force primes);;

let res = LazyList.(length % take_while (fun x -> x <= 16777216) @@ Lazy.force primes)

let () = Printf.printf "%d\n" res

(* $ ocamlbuild -r -use-ocamlfind -pkgs 'batteries' 'sieve.native' -tag 'optimize(3)' *)

-- F#

type LazyListCell<'a> = Nil | Cons of 'a * Lazy<LazyListCell<'a>>

type LazyList<'a> = Lazy<LazyListCell<'a>>

let rec map f (l : LazyList<'a>) = lazy (
    match l.Force() with
    | Nil -> Nil
    | Cons (x, xs) -> Cons (f x, map f xs))

let rec filter p (l : LazyList<'a>) = lazy (
    match l.Force() with
    | Nil -> Nil
    | Cons (x, xs) -> if p x then Cons (x, filter p xs) else (filter p xs).Force())

let rec takeWhile p (l : LazyList<'a>) = lazy (
    match l.Force() with
    | Nil -> Nil
    | Cons (x, xs) -> if p x then Cons (x, takeWhile p xs) else Nil)

let rec all p (l : LazyList<'a>) =
    match l.Force() with
    | Nil -> true
    | Cons (x, xs) -> if p x then all p xs else false

let rec enumFromDelta x d = lazy (Cons (x, enumFromDelta (x+d) d))

let length (xs : LazyList<'a>) =
    let rec go n (l : LazyList<'a>) =
        match l.Force() with
        | Nil -> n
        | Cons (x, xs) -> go (n + 1) xs
    go 0 xs

let rec primes = lazy (Cons (2, lazy (Cons (3, filter isPrime <| enumFromDelta 5 2))))
and     isPrime x = all ((<>) 0) << map (fun y -> x % y) << takeWhile ((fun y -> y <= x) << (fun x -> x*x)) <| primes

printf "%A" << length << takeWhile (fun x -> x <= 16777216) <| primes

-- автор klapaucius
-- первоисточник идеи http://thedeemon.livejournal.com/37515.html
