-- preliminary definitions

-- Cs :: PitchClass

-- concertA, a440 :: (PitchClass, Octave)
-- concertA = (A, 4)
-- a440 = (A, 4)

-- note :: Dur -> Pitch -> Music Pitch
-- rest :: Dur -> Pitch -> Music Pitch

-- (:+:) :: Music Pitch -> Music Pitch -> Music Pitch -- m1 followed by m2
-- (:=:) :: Music Pitch -> Music Pitch -> Music Pitch -- m1 and m2 simultanesouly

-- trans :: Int -> Pitch -> Pitch

-- mel = (note qn p1 :=: note qn (trans -3 p1)) :+:
--       (note qn p2 :=: note qn (trans -3 p2)) :+:      
--       (note qn p3 :=: note qn (trans -3 p3))

-- hNote :: Dur -> Pitch -> Music Pitch
-- hNote d p = note d p :=: note d (trans -3 p)

-- hList :: Dur -> [Pitch] -> Music Pitch
-- hList d [] = rest 0
-- hList d p : ps = hNote d p :+: hList d ps

-- mel = hList qn [p1, p2, p3]

-- exercise 1.4

-- hNote :: Dur -> Pitch -> Int -> Music Pitch
-- hNote d p n = note d p :=: note d (trans n p)

-- hList :: Dur -> [Pitch] -> Int -> Music Pitch
-- hList d [] n = rest 0
-- hList d (p : ps) n = hNote d p n :+: hList d ps n

-- mel n = hList qn n [p1, p2, p3]

--------------