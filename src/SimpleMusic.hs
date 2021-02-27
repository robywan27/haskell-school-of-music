-- module Euterpea.Music.Note.Music where
--     infixr 5:+:,:=:

-- type synonyms

type Octave = Int

type Pitch = (PitchClass, Octave)

type Dur = Rational

-- algebraic data type

data PitchClass = 
    Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds
    | Ef | Fff | Dss | E | Es | Ff | F | Gff | Ess | Fs
    | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As
    | Bf | Ass | B | Bs | Bss

data Primitive a =
    Note Dur a
    | Rest Dur

-- Note :: Dur -> a -> Primitive a
-- Rest :: Dur -> Primitive a

-- examples

qn :: Dur -- quarter note
qn = 1/4

concertA, a440 :: Pitch
concertA = (A, 4)
a440 = (A, 4)

note1 :: Primitive Pitch
note1 = Note qn concertA

note2 :: Primitive Pitch
note2 = Note qn (G 4)

rest1 :: Primitive
rest1 = Rest qn

----------------

data Music a =
    Prim (Primitive a)          -- primitive value
    | Music a :+: Music a       -- sequential composition
    | Music a :=: Music a       -- parallele composition
    | Modify Control (Music a)  -- modifier

-- Prim :: Primitive a -> Music a
-- (:+:) :: Music a -> Music a -> Music a
-- (:=:) :: Music a -> Music a -> Music a
-- Modify :: Control -> Music a -> Music a

-- examples

a440m :: Music Pitch
a440m = Prim (Note qn a440)

-----------

data Control =
    Tempo Rational              -- scale the tempo
    | Transpose AbsPitch        -- transposition
    | Instrument InstrumentName -- instrument label
    | Phrase [PhraseAttribute]  -- phrase attributes
    | Player PlayerName         -- player label

type PlayerName = String

-- convenient auxiliary functions

note :: Dur -> a -> Music a
note d p = Prim (Note d p)

rest :: Dur -> Music a
rest d = Prim (Rest d)

tempo :: Dur -> Music a -> Music a
tempo r m = Modify (Tempo r) m

transpose :: AbsPitch -> Music a -> Music a
transpose p m = Modify (Transpose p) m

instrument :: InstrumentName -> Music a -> Music a
instrument i m = Modify (Instrument i) m

phrase :: [PhraseAttribute] -> Music a -> Music a
phrase as m = Modify (Phrase as) m

player :: PlayerName -> Music a -> Music a
player n m = Modify (PlayerName n) m

data InstrumentName =
    AcousticGrandPiano | BrightAcousticPiano | ElectricGrandPiano
    | HonkyTonkPiano | RhodesPiano | ChorusedPiano
    | Harpsichord | Clavinet | Celesta
    | Glockenspiel | MusicBox | Vibraphone
    | Marimba | Xylophone | TubularBells
    | Dulcimer | HammondOrgan | PercussiveOrgan
    | RockOrgan | ChurchOrgan | ReedOrgan
    | Accordion | Harmonica | TangoAccordion
    | AcousticGuitarNylon | AcousticGuitarSteel | ElectricGuitarJazz
    | ElectricGuitarClean | ElectricGuitarMuted | OverdrivenGuitar
    | DistortionGuitar | GuitarHarmonics | AcousticBass
    | ElectricBassFingered | ElectricBassPicked | FretlessBass
    | SlapBass1 | SlapBass2 | SynthBass1
    | SynthBass2 | Violin | Viola
    | Cello | Contrabass | TremoloStrings
    | PizzicatoStrings | OrchestralHarp | Timpani
    | StringEnsemble1 | StringEnsemble2 | SynthStrings1
    | SynthStrings2 | ChoirAahs | VoiceOohs
    | SynthVoice | OrchestraHit | Trumpet
    | Trombone | Tuba | MutedTrumpet
    | FrenchHorn | BrassSection | SynthBrass1
    | SynthBrass2 | SopranoSax | AltoSax
    | TenorSax | BaritoneSax | Oboe
    | Bassoon | EnglishHorn | Clarinet
    | Piccolo | Flute | Recorder
    | PanFlute | BlownBottle | Shakuhachi
    | Whistle | Ocarina | Lead1Square
    | Lead2Sawtooth | Lead3Calliope | Lead4Chiff
    | Lead5Charang | Lead6Voice | Lead7Fifths
    | Lead8BassLead | Pad1NewAge | Pad2Warm
    | Pad3Polysynth | Pad4Choir | Pad5Bowed
    | Pad6Metallic | Pad7Halo | Pad8Sweep
    | FX1Train | FX2Soundtrack | FX3Crystal
    | FX4Atmosphere | FX5Brightness | FX6Goblins
    | FX7Echoes | FX8SciFi | Sitar
    | Banjo | Shamisen | Koto
    | Kalimba | Bagpipe | Fiddle
    | Shanai | TinkleBell | Agogo
    | SteelDrums | Woodblock | TaikoDrum
    | MelodicDrum | SynthDrum | ReverseCymbal
    | GuitarFretNoise | BreathNoise | Seashore
    | BirdTweet | TelephoneRing | Helicopter
    | Applause | Gunshot | Percussion
    | Custom String

cff , cf , c, cs, css, dff , df , d, ds, dss, eff , ef , e, es, ess, fff , ff , f ,
    fs, fss, gff , gf , g, gs, gss, aff , af , a, as, ass, bff , bf , b, bs, bss ::
        Octave -> Dur -> Music Pitch
cff o d = note d (Cff , o); cf o d = note d (Cf , o)
c o d = note d (C, o); cs o d = note d (Cs, o)
css o d = note d (Css, o); dff o d = note d (Dff , o)
df o d = note d (Df , o); d o d = note d (D, o)
ds o d = note d (Ds, o); dss o d = note d (Dss, o)
eff o d = note d (Eff , o); ef o d = note d (Ef , o)
e o d = note d (E, o); es o d = note d (Es, o)
ess o d = note d (Ess, o); fff o d = note d (Fff , o)
ff o d = note d (Ff , o); f o d = note d (F, o)
fs o d = note d (Fs, o); fss o d = note d (Fss, o)
gff o d = note d (Gff , o); gf o d = note d (Gf , o)
g o d = note d (G, o); gs o d = note d (Gs, o)
gss o d = note d (Gss, o); aff o d = note d (Aff , o)
af o d = note d (Af , o); a o d = note d (A, o)
as o d = note d (As, o); ass o d = note d (Ass, o)
bff o d = note d (Bff , o); bf o d = note d (Bf , o)
b o d = note d (B, o); bs o d = note d (Bs, o)
bss o d = note d (Bss, o)

bn, wn, hn, qn, en, sn, tn, sfn, dwn, dhn,
    dqn, den, dsn, dtn, ddhn, ddqn, dden :: Dur
bnr, wnr , hnr , qnr , enr, snr, tnr, dwnr , dhnr ,
    dqnr , denr , dsnr , dtnr, ddhnr , ddqnr , ddenr :: Music Pitch
bn = 2; bnr = rest bn -- brevis rest
wn = 1; wnr = rest wn -- whole note rest
hn = 1/2; hnr = rest hn -- half note rest
qn = 1/4; qnr = rest qn -- quarter note rest
en = 1/8; enr = rest en -- eighth note rest
sn = 1/16; snr = rest sn -- sixteenth note rest
tn = 1/32; tnr = rest tn -- thirty-second note rest
sfn = 1/64; sfnr = rest sfn -- sixty-fourth note rest
dwn = 3/2; dwnr = rest dwn -- dotted whole note rest
dhn = 3/4; dhnr = rest dhn -- dotted half note rest
dqn = 3/8; dqnr = rest dqn -- dotted quarter note rest
den = 3/16; denr = rest den -- dotted eighth note rest
dsn = 3/32; dsnr = rest dsn -- dotted sixteenth note rest
dtn = 3/64; dtnr = rest dtn -- dotted thirty-second note rest
ddhn = 7/8; ddhnr = rest ddhn -- double-dotted half note rest
ddqn = 7/16; ddqnr = rest ddqn -- double-dotted quarter note rest
dden = 7/32; ddenr = rest dden -- double-dotted eighth note rest

-- chord progression II-V-I on C4 example

t251 :: Music Pitch
t251 =
    let dMinor = d 4 wn :=: f 4 wn :=: a 3 wn
        gMajor = g 4 wn :=: b 4 wn :=: d 5 wn
        cMajor = c 4 wn :=: e 4 wn :=: g 4 wn
    in dMinor :+: gMajor :+: cMajor

-----------------

type AbsPitch = Int

absPitch :: Pitch -> AbsPitch
absPitch (p, oct) = 12 * oct + pcToInt p

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

pitch :: AbsPitch -> Pitch
pitch ap =
    let (oct, n) = divMod ap 12
    in ([C, Cs,D, Ds,E, F, Fs,G, Gs,A, As,B ] !! n, oct)

trans :: Int -> Pitch -> Pitch
trans i p = pitch (absPitch p + i)

-- exercise 2.1

twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne p d =
    let minorSecond = note d p :=: note d (trans 3 p) :=: note d (trans 7 p)
        majorFifth = note d (trans 5 p) :=: note d (trans 8 p) :=: note d (trans 12 p)
        majorFirst = note d*2 (trans -2 p) :=: note d*2 (trans 2 p) :=: note d*2 (trans 5 p)
    in minorSecond :+: majorFifth :+: majorFirst
twoFiveOne (D, 4) wn = t251

--------------
