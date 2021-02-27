type Octave = Int

type Pitch = (PitchClass, Octave)

type Dur = Rational

data PitchClass = 
    Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
    | Ef | Fff | Dss | E | Es | Ff | F | Gff | Ess | Fs
    | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
    | Bf | Ass | B | Bs | Bss

data Primitive a =
    Note Dur a
    | Rest Dur

type Music a = Primitive a

note :: Dur -> a -> Music a
note = Note

rest :: Dur -> Music a
rest = Rest

qn = 1/4; qnr = rest qn

type AbsPitch = Int

absPitch :: Pitch -> AbsPitch
absPitch (p, oct) = 12 * oct + pcToInt p

pitch :: AbsPitch -> Pitch
pitch ap =
    let (oct, n) = divMod ap 12
    in ([C, Cs,D, Ds,E, F, Fs,G, Gs,A, As,B ] !! n, oct)

pcToInt :: PitchClass -> Int
pcToInt Cff = -2; pcToInt Dff = 0; pcToInt Eff = 2
pcToInt Cf = -1; pcToInt Df = 1; pcToInt Ef = 3
pcToInt C = 0; pcToInt D = 2; pcToInt E = 4
pcToInt Cs = 1; pcToInt Ds = 3; pcToInt Es = 5
pcToInt Css = 2; pcToInt Dss = 4; pcToInt Ess = 6
pcToInt Fff = 3; pcToInt Gff = 5; pcToInt Aff = 7
pcToInt Ff = 4; pcToInt Gf = 6; pcToInt Af = 8
pcToInt F = 5; pcToInt G = 7; pcToInt A = 9
pcToInt Fs = 6; pcToInt Gs = 8; pcToInt As = 10
pcToInt Fss = 7; pcToInt Gss = 9; pcToInt Ass = 11
pcToInt Bff = 9
pcToInt Bf = 10
pcToInt B = 11
pcToInt Bs = 12
pcToInt Bss = 13

-- rewriting some of the previosuly defined functions when given lists

-- map :: (a -> b) -> [a] -> [b]
-- map f [] = []
-- map f (x : xs) = f x : map f xs

-- toAbsPitches :: [Pitch] -> [AbsPitch]
-- toAbsPitches ps = map absPitch ps

-- toPitches :: [AbsPitch] -> [Pitch]
-- toPitches ps = map pitch ps

-- whole-tone scale

wts :: Pitch -> [Music Pitch]
wts p =
    let f ap = note qn pitch (absPitch p + ap)
    in map f [0, 2, 4, 6, 8]

----------

-- (++) :: [a] -> [a] -> [a]
-- [] ++ ys = ys
-- (x : xs) ++ ys = x : xs ++ ys

-- melody, chord, maxPitch

-- line :: [Music a] -> Music a
-- line [] = rest 0
-- line (m : ms) = m :+: line ms

-- chord :: [Music a] -> Music a
-- chord [] = rest 0
-- chord (m : ms) = m :=: chord ms

-- maxPitch :: [Pitch] -> Pitch
-- maxPitch [] = pitch 0
-- maxPitch (p : ps) = p !!! maxPitch ps

p1 !!! p2 = if absPitch p1 > absPitch p2 then p1 else p2

----------

fold :: (a -> b -> b) -> b -> [a] -> b
fold op init [] = init
fold op init (x : xs) = x `op` fold op init xs

-- melody, chord, maxPitch, hList with fold

-- line ms = fold (:+:) (rest 0) ms

-- chord ms = fold (:=:) (rest 0) ms

-- maxPitch ps = fold (!!!) (pitch 0) ps

-- hList d ps = 
--     let f p = hNote d p 
--     in fold (:+:) (rest 0) (map f ps)
-- hList d ps = 
--     let f p = hNote d p 
--     in line (map f ps)

-------------

-- foldr :: (a -> b -> b) -> b -> [a] -> b -- same definition as previously defined fold
-- foldr op init [] = init
-- foldr op init (x : xs) = x `op` foldr op init xs

-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl op init [] = init
-- foldl op init (x : xs) = foldl op (init `op` x) xs

-- operations on non-empty lists

-- line1 ms = foldr1 (:+:) ms

-- chord1 ms = foldr1 (:=:) ms

-- maxPitch1 ps = foldr1 (!!!) ps

------------

-- currying

line :: [Music a] -> Music a
line = fold (:+:) (rest 0)

chord :: [Music a] -> Music a
chord = fold (:=:) (rest 0)

maxPitch :: [Pitch] -> Pitch
maxPitch = fold (!!!) (pitch 0)

toAbsPitches :: [Pitch] -> [AbsPitch]
toAbsPitches = map absPitch

toPitches :: [AbsPitch] -> [Pitch]
toPitches = map pitch

hList :: Dur → [Pitch ] → Music Pitch
hList d ps = line (map (hNote d) ps)
hList = line (map hNote)

line1 :: [Music a] -> Music a
line1 = foldr1 (:+:)

chord1 :: [Music a] -> Music a
chord1 = foldr1 (:=:)

maxPitch1 :: [Pitch] -> Pitch
maxPitch1 = foldr1 (!!!)

-----------

-- errors

foldr1err :: (a -> a -> a) -> [a] -> a
foldr1err f [x] = x
foldr1err f (x : xs) = f x (foldr1err f xs)
foldr1err f [] = error "Prelude.foldr1: empty list"

---------

-- exercise 3.6

lnegth :: [a] -> Int
length [] = 0
length (_ : xs) = 1 + length xs
length xs = 
    let f _ = 1
    in foldr (+) 0 (map f xs)

--------------

-- exercise 3.1

flip :: (a → b → c) → (b → a → c)
flip f x y = f y x

-- demonstrate flip (flip f) = f
-- flip (f y x) = f x y = f

---------------

-- exercise 3.2

xs = [1,2,3] :: [Integer]
-- ((+) x) y = x + y      only one value x is passed to binary operator (+),
-- so it is returned a curried function that expects an integer y and returns the sum of x and y, an integer.
-- It is a list of functions because map returns a list
ys :: [Integer -> Integer]
ys = map (+) xs

---------------

-- exercise 3.3

applyEach :: [a -> b] -> a -> [b]
applyEach [] v = []
applyEach (f : fs) v = f v : applyEach fs v

---------------

-- exercise 3.4 DOES NOT COMPILE

applyAll :: [a -> a] -> a -> a
-- applyAll (f : fs) v = foldl (f v) v [fs v]
applyAll f v = f v
applyAll (f : fs) v = f applyAll fs v

---------------

-- exercise 3.7

doubleEach :: [Int] -> [Int]
doubleEach = map (*2)

pairAndOne :: [Int] -> [(Int, Int)]
pairAndOne = map (\n -> (n, n+1))

addEachPair :: [(Int, Int)] -> [Int]
addEachPair [] = []
addEachPair ((n1, n2) : ns) = (n1 + n2) : addEachPair ns

-- addPairsPointwise :: [(Int, Int)] -> (Int, Int)
-- addPairsPointwise ns = foldl (+) (0, 0) ns

--------------

-- exercise 3.9 DOES NOT COMPILE

chrom :: Pitch -> Pitch -> [Music Pitch]
chrom p1 p2 =
    let f x y = if x > y then [] else note qn x : f (x + 1) y
    in if p1 < p2 then f p1 p2
    else if p1 > p2 then f p2 p1
    else note qn p1

---------------
