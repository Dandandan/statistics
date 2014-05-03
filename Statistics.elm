module Statistics where
import Dict
import Maybe

mean : [Float] -> Float
mean xs =
    sum xs / toFloat (length xs)

median : [Float] -> Float
median xs =
    let l = length xs
        x = xs |> drop (l `div` 2 - 1)
    in if l `mod` 2 == 0 then xs |> drop (l `div` 2 - 1) |> take 2 |> mean
                         else xs |> drop (l `div` 2) |> head

mode : [comparable] -> comparable
mode xs =
    let count y ys = Dict.update y (Maybe.maybe (Just 1) (\c -> (Just (c + 1)))) ys
        counts      = foldr count Dict.empty xs
    in Dict.toList counts |> sortWith (\x y -> (flip compare) (snd x) (snd y)) |> head |> fst


