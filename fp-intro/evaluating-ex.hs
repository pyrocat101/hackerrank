import Text.Printf

integrateSeries s = zipWith (/) s [1..]
expSeries = 1.0 : integrateSeries expSeries

e x = sum $ zipWith (*) (take 10 expSeries) (iterate (* x) 1)

happy 0 = return ()
happy n = do
  xStr <- getLine
  let x = read xStr :: Double
  printf "%.4f\n" $ e x
  happy (n - 1)

main = do
  nStr <- getLine
  let n = read nStr :: Integer
  happy n
