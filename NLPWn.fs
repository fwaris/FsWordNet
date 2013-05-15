module NLPWn
open System
open System.IO
open NLPWTree
let path = @"C:\ws\nlp\wn"
let nounIdx = Path.Combine(path,"index.noun")
let verbIdx = Path.Combine(path,"index.verb")
let adjIdx = Path.Combine(path,"index.adj")
let advIdx = Path.Combine(path,"index.adv")

let nounExc = Path.Combine (path,"noun.exc")
let verbExc = Path.Combine (path,"verb.exc")
let adjExc = Path.Combine(path,"adj.exc")
let advExc = Path.Combine(path,"adv.exc")

type IndexEntry = {Lemma:string; Pos:char; SynsetCount:int; PointerCount:int; Pointers:string list; TagSenseCount:int; Offset:Int64}

let take i f (s:string) = 
    let rec loop i acc =
        if i < s.Length then
            let c = s.[i]
            if f c then loop (i+1) (c::acc)
            else String(acc |> List.rev |> List.toArray),(i,s)
        else String(acc |> List.rev |> List.toArray),(i,s)
    loop i []

let isLetterOrDigit = Char.IsLetterOrDigit
let isWhitespace = Char.IsWhiteSpace
let isNotWS = isWhitespace>>not

let (|WS|) (i,s) = s |> take i isWhitespace |> snd
let (|Char|_|) c (i,s:string)  = 
    if i < s.Length then 
        if s.[i] = c then Some(c,(i+1,s)) else None 
    else None

let (|Token|) (i,s) = take i isNotWS s
let (|Pos|) = function Char 'n' x | Char 'v' x | Char 'a' x | Char 'r' x -> x | _ -> failwith "invalid char"
let (|Count|) = function Token (c,(i,s)) -> Int32.Parse(c),(i,s)
let (|Offset|) = function Token (c,(i,s)) -> Int64.Parse(c),(i,s)
let (|Pointers|) (count,st) = 
    let rec loop st j acc =
        if j = 0 then acc |> List.rev,st
        else 
            match st with
            | WS (Token (c, rest)) -> loop rest (j-1) (c::acc)
    loop st count []

let toIndexEntry (line:string) =
    match (0,line) with
    | Token (lemma, WS (Pos (p, WS (Count (synsets, WS (Count (Pointers (ptrs, WS (Count (_, WS (Count (tagCount, WS (Offset (off,_)))))))))))))) ->
        {
            Lemma = lemma.Replace("_"," ")
            Pos = p
            SynsetCount=synsets
            PointerCount = ptrs|>List.length
            Pointers = ptrs
            TagSenseCount = tagCount
            Offset = off
        }

let indexedNouns =
    nounIdx
    |> File.ReadLines
    |> Seq.filter (fun l -> l.IndexOf(' ') = 0 |> not)
    |> Seq.map toIndexEntry
    |> Seq.toList

let indexedVerbs =
    verbIdx
    |> File.ReadLines
    |> Seq.filter (fun l -> l.IndexOf(' ') = 0 |> not)
    |> Seq.map toIndexEntry
    |> Seq.toList

let indexedAdjectives =
    adjIdx
    |> File.ReadLines
    |> Seq.filter (fun l -> l.IndexOf(' ') = 0 |> not)
    |> Seq.map toIndexEntry
    |> Seq.toList

let indexedAdverbs =
    advIdx
    |> File.ReadLines
    |> Seq.filter (fun l -> l.IndexOf(' ') = 0 |> not)
    |> Seq.map toIndexEntry
    |> Seq.toList

let idxN = (T.Empty,indexedNouns) ||> List.fold (fun t n -> t |> WTree.add n.Lemma noun)
let idxV = (idxN,indexedVerbs) ||> List.fold(fun t n -> t |> WTree.add n.Lemma verb)
let idxAdv = (idxV,indexedAdverbs) ||> List.fold(fun t n -> t |> WTree.add n.Lemma adverb)
let idxAdj = (idxAdv,indexedAdjectives) ||> List.fold(fun t n -> t |> WTree.add n.Lemma adverb)

let addExceptions path ps root  =
    (root,path|>File.ReadLines) ||> Seq.fold (fun s l ->
        let word_lemma = l.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
        s |> WTree.addWithException word_lemma.[0] ps (Some word_lemma.[1]) 
        )

let allIdx = (idxAdj, [nounExc,noun; verbExc,verb; adjExc, adjective; advExc,adverb]) ||> List.fold(fun r (n,ps) -> r |> addExceptions n ps)

(*
char *ptrtyp[]={
    "",				/* 0 not used */
    "!",			/* 1 ANTPTR */
    "@",			/* 2 HYPERPTR */
    "~",			/* 3 HYPOPTR */
    "*",			/* 4 ENTAILPTR */
    "&",			/* 5 SIMPTR */
    "#m",			/* 6 ISMEMBERPTR */
    "#s",			/* 7 ISSTUFFPTR */
    "#p",			/* 8 ISPARTPTR */
    "%m",			/* 9 HASMEMBERPTR */
    "%s",			/* 10 HASSTUFFPTR */
    "%p",			/* 11 HASPARTPTR */
    "%",			/* 12 MERONYM */
    "#",			/* 13 HOLONYM */
    ">",			/* 14 CAUSETO */
    "<",			/* 15 PPLPTR */
    "^",			/* 16 SEEALSO */
    "\\",			/* 17 PERTPTR */
    "=",			/* 18 ATTRIBUTE */
    "$",			/* 19 VERBGROUP */
    "+",		        /* 20 NOMINALIZATIONS */
    ";",			/* 21 CLASSIFICATION */
    "-",			/* 22 CLASS */
/* additional searches, but not pointers.  */
    "",				/* SYNS */
    "",				/* FREQ */
    "+",			/* FRAMES */
    "",				/* COORDS */
    "",				/* RELATIVES */
    "",				/* HMERONYM */
    "",				/* HHOLONYM */
    "",				/* WNGREP */
    "",				/* OVERVIEW */
    ";c",			/* CLASSIF_CATEGORY */
    ";u",			/* CLASSIF_USAGE */
    ";r",			/* CLASSIF_REGIONAL */
    "-c",			/* CLASS_CATEGORY */
    "-u",			/* CLASS_USAGE */
    "-r",			/* CLASS_REGIONAL */
    "@i",			/* INSTANCE */
    "~i",			/* INSTANCES */
    NULL,
};
*)

