module Main exposing (main)

import Html exposing (text, div, p)
import Emagma exposing (..)

logt s lst = (s |> str |> text) :: p[][] :: lst
logs s lst = (s |> text) :: p[][] :: lst

logReduce t = case rho t of
  Reduct True tt ->
    let (html, reduct) = logReduce tt in
        (logt tt html, reduct)
  Reduct False tt -> (logt tt [], tt) 

(v, w, x, y, z) = (b 4, b 3, b 2, b 1, b 0)

zero   = lm [y, z] z
succ   = lm [x, y, z] (ap [y, ap [x, y, z]])
add    = lm [w, x, y, z] (ap [w, y, ap [x, y, z]])
mul    = lm [x, y, z] (ap [x, ap [y, z]])
bTrue  = lm [1, 0] (b 1)
bFalse = lm [1, 0] (b 0)
fst    = lm [0] (ap [b 0, bTrue])
snd    = lm [0] (ap [b 0, bFalse])
phi    = lm [1, 0] (ap [b 0, ap [succ, ap [fst, b 1]], ap [fst, b 1]])
pred   = lm [0] (ap [snd, ap [b 0, phi, lm [0] (ap [b 0, zero, zero])]])
sub    = lm [1, 0] (ap [b 0, pred, b 1])

one = reduce (ap [succ, zero])
two = reduce (ap [succ, one])
three = reduce (ap [succ, two])

main =
    let term = ap [sub, three, two] in
    let (html, _) = logReduce term in
        div [] (logs ("START: " ++ (str term)) html)
