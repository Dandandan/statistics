{- Module Statistics

  Copyright (c) 2013 Steven D'Aprano <steve+python@pearwood.info>.
                2014 Daniël Heres
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at

  http://www.apache.org/licenses/LICENSE-2.0

  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}
module Statistics where
{-| This module provides functions for calculating mathematical statistics of numeric data.

# Averages and measures of central location
These functions calculate an average or typical value from a population or sample.
@docs mean, median, medianLow, medianHigh, mode

#Measures of spread
These functions calculate a measure of how much the population or sample tends to deviate from the typical or average values.
@docs variance, populationVariance, standardDeviation, populationStandardDeviation
-}

import Dict
import Maybe

{-| Return the sample arithmetic mean of a list of real-valued numbers.

The arithmetic mean is the sum of the data divided by the number of data points. It is commonly called “the average”, although it is only one of many different mathematical averages. It is a measure of the central location of the data.

    mean [1, 2, 3, 4, 4]         == 2.8
    mean [-1.0, 2.5, 3.25, 5.75] == 2.625

-}
mean : [Float] -> Float
mean xs =
    sum xs / toFloat (length xs)

{-| Return the median (middle value) of numeric data, using the common “mean of middle two” method.

The median is a robust measure of central location, and is less affected by the presence of outliers in your data. When the number of data points is odd, the middle data point is returned:

    median [1, 3, 5]    == 3
    median [1, 3, 5, 7] == 4
-}
median : [Float] -> Float
median xs =
    let l = length xs
        x = sort xs
    in if l `mod` 2 == 0 then x |> drop (l `div` 2 - 1) |> take 2 |> mean
                         else x |> drop (l `div` 2) |> head

{-| Return the low median of numeric data.

The low median is always a member of the data set. When the number of data points is odd, the middle value is returned. When it is even, the smaller of the two middle values is returned.

    medianLow [1, 3, 5]    == 3
    medianLow [1, 3, 5, 7] == 3
-}
medianLow : [Float] -> Float
medianLow xs =
    let l = length xs
        x = sort xs
    in if l `mod` 2 == 0 then x |> drop (l `div` 2 - 1) |> head
                         else x |> drop (l `div` 2) |> head


{-| Return the high median of data.

The high median is always a member of the data set. When the number of data points is odd, the middle value is returned. When it is even, the larger of the two middle values is returned.

    medianHigh [1, 3, 5]    == 3
    medianHigh [1, 3, 5, 7] == 5
-}
medianHigh : [Float] -> Float
medianHigh xs =
    let l = length xs
        x = sort xs
    in x |> drop (l `div` 2) |> head

{-| Return the most common data point from discrete or nominal data. The mode (when it exists) is the most typical value, and is a robust measure of central location.

`mode` assumes discrete data, and returns a single value. This is the standard treatment of the mode as commonly taught in schools:

    mode [1, 1, 2, 3, 3, 3, 3, 4]        == 3
    mode ["red", "blue", "green", "red"] == "red"
-}
mode : [comparable] -> comparable
mode xs =
    let count y ys = Dict.update y (Maybe.maybe (Just 1) (\c -> (Just (c + 1)))) ys
        counts      = foldr count Dict.empty xs
    in Dict.toList counts |> sortWith (\x y -> (flip compare) (snd x) (snd y)) |> head |> fst

{-| Return the sample variance of data, an iterable of at least two real-valued numbers. Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of data. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.

Use this function when your data is a sample from a population. To calculate the variance from the entire population, see `populationVariance`.

    variance [2.75, 1.75, 1.25, 0.25, 0.5, 1.25, 3.5] == 1.3720238095238095

-}
variance : [Float] -> Float
variance xs =
    let n  = length xs
        c  = mean xs
        ss = map (\x -> (x - c) ^ 2) xs |> sum
    in 
        ss / (toFloat n - 1)


{-| Return the population variance of a list of real-valued numbers. Variance, or second moment about the mean, is a measure of the variability (spread or dispersion) of data. A large variance indicates that the data is spread out; a small variance indicates it is clustered closely around the mean.

Use this function to calculate the variance from the entire population. To estimate the variance from a sample, `variance` is usually a better choice.

     populationVariance [0.0, 0.25, 0.25, 1.25, 1.5, 1.75, 2.75, 3.25] == 1.25
-}
populationVariance : [Float] -> Float
populationVariance xs =
    let n  = length xs
        c  = mean xs
        ss = map (\x -> (x - c) ^ 2) xs |> sum
    in 
        ss / toFloat n

{-| Return the sample standard deviation (the square root of the sample variance).

    standardDeviation [1.5, 2.5, 2.5, 2.75, 3.25, 4.75] == 1.0810874155219827
-}
standardDeviation : [Float] -> Float
standardDeviation xs =
    variance xs |> sqrt

{-| Return the population standard deviation (the square root of the population variance).

    populationStandardDeviation [1.5, 2.5, 2.5, 2.75, 3.25, 4.75] == 0.986893273527251
-}
populationStandardDeviation : [Float] -> Float
populationStandardDeviation xs =
    populationVariance xs |> sqrt
