module Emagma exposing (..)

type Term = Bound Int | Lambda Term | App Term Term

sigma: Int -> Int-> Term -> Term
sigma x y tt = case tt of 
  Bound z -> if z >= x then Bound (z + y) else Bound z          -- аксиома (сигма3)
  Lambda p -> Lambda (sigma (x+1) y p)                          -- аксиома (сигма2)
  App p q -> App (sigma x y p) (sigma x y q)                    -- аксиома (сигма1)

lambda: Int-> Term -> Term -> Term
lambda x p q  = case p of
  Bound y -> if x == y then sigma 0 x q else p                  -- аксиома (лямбда3)
  Lambda t -> Lambda (lambda (x+1) t q)                         -- аксиома (лямбда2)
  App t1 t2 -> App (lambda x t1 q) (lambda x t2 q)              -- аксиома (лямбда1)

type Reduct = Reduct Bool Term

rho: Term -> Reduct
rho p = case p of
  Bound y -> Reduct False (Bound y)                             -- аксиома (редукция1)
  Lambda t -> let (Reduct b tt) = rho t in Reduct b (Lambda tt) -- аксиома (редукция2)
  App t1 t2 -> 
    let
      (Reduct b1 tt1) = rho t1
      (Reduct b2 tt2) = rho t2
    in
      if b1 || b2 then Reduct True (App tt1 tt2)                -- аксиома (редукция3)
      else case t1 of
        Lambda q -> Reduct True (lambda 0 (sigma 1 -1 q) t2)    -- аксиома (редукция4)
        _ -> Reduct False p

str: Term -> String
str t = case t of
  Bound x -> toString x
  Lambda p -> "λ.(" ++ (str p) ++ ")"
  App p q -> "(" ++ (str p) ++ " " ++ (str q) ++ ")"

reduce: Term -> Term
reduce t = case rho t of
  Reduct True tt -> reduce tt
  Reduct False tt -> tt 

lm x t = case x of
  b :: [] -> Lambda t
  b :: c :: xs -> Lambda (lm (c::xs) t)
  _ -> t

ap x = case x of
  a :: [] -> a
  a :: b :: xs -> ap ((App a b) :: xs)
  _ -> lm [0] (b 0)

b x = Bound x