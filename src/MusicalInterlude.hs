module HSoM.Examples.Interlude (
    twinkle, -- :: Music Pitch
    childSong6, -- :: Music Pitch
    prefixes -- :: [a] -> [[a]]
    ) where
import Euterpea

-- import Debug.Trace

-- prefixes :: Show a => [a] -> [[a]]
-- prefixes [ ] = [ ]
-- prefixes (x : xs) = 
--     let f pf = trace ("In function: x = " ++ show x ++ ", pf = " ++ show pf) $ x : pf
--     in trace ("In recursion: x = " ++ show x ++ ", xs = " ++ show xs) $ [x] : map f (prefixes xs)

-- twinkle twinkle lilttle star

twinkle1 =
    c 4 qn :+: c 4 qn :+: g 4 qn :+: g 4 qn :+: a 4 qn :+: a 4 qn :+: g 4 hn :+:
    f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 qn :+: d 4 qn :+: c 4 hn :+:
    g 4 qn :+: g 4 qn :+: f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 hn :+:
    g 4 qn :+: g 4 qn :+: f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 hn :+:
    c 4 qn :+: c 4 qn :+: g 4 qn :+: g 4 qn :+: a 4 qn :+: a 4 qn :+: g 4 hn :+:
    f 4 qn :+: f 4 qn :+: e 4 qn :+: e 4 qn :+: d 4 qn :+: d 4 qn :+: c 4 hn

pcToQn :: PitchClass -> Music Pitch
pcToQn pc = note qn (pc, 4)

twinkle2 =
    line (map pcToQn [C, C, G, G, A, A]) :+: g 4 hn :+:
    line (map pcToQn [F, F, E, E, D, D]) :+: c 4 hn :+:
    line (map pcToQn [G, G, F, F, E, E]) :+: d 4 hn :+:
    line (map pcToQn [G, G, F, F, E, E]) :+: d 4 hn :+:
    line (map pcToQn [C, C, G, G, A, A]) :+: g 4 hn :+:
    line (map pcToQn [F, F, E, E, D, D]) :+: c 4 hn

twinkle =
    let m1 = line (map pcToQn [C, C, G, G, A, A]) :+: g 4 hn
        m2 = line (map pcToQn [F, F, E, E, D, D]) :+: c 4 hn
        m3 = line (map pcToQn [G, G, F, F, E, E]) :+: d 4 hn
    in line [m1, m2, m3, m3, m1, m2]

-------------


-- children's song no. 6 - chick corea

times2 :: Int -> Music a -> Music a      -- times clashes with Euterpea's times function, so here it is renamed times2
times2 0 m = rest 0
times2 n m = m :+: times2 (n - 1) m

addDur :: Dur -> [Dur -> Music a] -> Music a    -- remember (a 4 qn :: Octave -> Dur -> Music a) so because of currying (a 4 :: Dur -> Music a)
addDur d ns =
    let f n = n d
    in line (map f ns)

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote n (Prim (Note d p)) = note (d / 8) (trans n p) :+: note (7 * d / 8) p
graceNote n _ = error "Can only add a grace note to a note."

-- bass line

b1 = addDur dqn [b 3, fs 4, g 4, fs 4]
b2 = addDur dqn [b 3, es 4, fs 4, es 4]
b3 = addDur dqn [as 3, fs 4, g 4, fs 4]

bassLine = times2 3 b1 :+: times2 2 b2 :+: times2 4 b3 :+: times2 5 b1

-------------

-- main voice

mainVoice = times2 3 v1 :+: v2

v1 = v1a :+: graceNote (-1) (d 5 qn) :+: v1b -- bars 1-2
v1a = addDur en [a 5, e 5, d 5, fs 5, cs 5, b 4, e 5, b 4]
v1b = addDur en [cs 5, b 4]

v2 = v2a :+: v2b :+: v2c :+: v2d :+: v2e :+: v2f :+: v2g

v2a = line [cs 5 (dhn + dhn), d 5 dhn, 
    f 5 hn, gs 5 qn, fs 5 (hn + en), g 5 en] -- bars 7–11
v2b = addDur en [fs 5, e 5, cs 5, as 4] :+: a 4 dqn :+:
    addDur en [as 4, cs 5, fs 5, e 5, fs 5] -- bars 12–13
v2c = line [g 5 en, as 5 en, cs 6 (hn + en), d 6 en, cs 6 en] :+:
    e 5 en :+: enr :+:
    line [as 5 en, a 5 en, g 5 en, d 5 qn, c 5 en, cs 5 en] -- bars 14–16
v2d = addDur en [fs 5, cs 5, e 5, cs 5,
    a 4, as 4, d 5, e 5, fs 5] -- bars 17–18.5
v2e = line [graceNote 2 (e 5 qn), d 5 en, graceNote 2 (d 5 qn), cs 5 en,
    graceNote 1 (cs 5 qn), b 4 (en + hn), cs 5 en, b 4 en] -- bars 18.5–20
v2f = line [fs 5 en, a 5 en, b 5 (hn + qn), a 5 en, fs 5 en, e 5 qn,
    d 5 en, fs 5 en, e 5 hn, d 5 hn, fs 5 qn] -- bars 21–23
v2g = tempo (3/2) (line [cs 5 en, d 5 en, cs 5 en]) :+:
    b 4 (3 * dhn + hn) -- bars 24–28

---------------

childSong6 :: Music Pitch
childSong6 =
    let t = (dhn/qn) * (69/120)
    in instrument RhodesPiano (tempo t (mainVoice :=: bassLine))



-- algorithmic composition

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x : xs) =
    let f pf = x : pf
    in [x] : map f (prefixes xs)

prefix :: [Music a] -> Music a
prefix mel =
    let m1 = line (concat (prefixes mel))
        m2 = line (concat (prefixes (reverse mel)))
        m = instrument Harpsichord m1 :=: instrument Accordion m2
    in m :+: transpose 5 m :+: m