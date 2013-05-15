module NLPWTree
open System
type Pos = N | V | Adj | Adv
let noun = Set[N]
let verb = Set[V]
let adjective = Set[Adj]
let adverb = Set[Adv]
type T = T of (Map<char,T>*string option*Set<Pos>) | Empty
let tStr t = match t with T(_,s,_) -> s
let tMap t = match t with T(m,_,_) -> m
let tPos t = match t with T(_,_,ps) -> ps
let tChild c (T(map,_,_))  = map.[Char.ToLower(c)]

let empty = Empty

[<RequireQualifiedAccess>]
module WTree =

    let private createFromEmpty (cs:char list) s ps = 
        let cs' = cs |> List.rev
        match cs with
        | [] -> Empty
        | _ -> 
            (T(Map.empty,s,ps),cs')
            ||> List.fold (fun t c -> T(Map.empty |> Map.add c t,None, Set.empty))

    let rec private searchDown t unmatchedCS accT accC =
        let m = tMap t
        match unmatchedCS with
        | [] -> (t::accT),accC,unmatchedCS
        | x::rest -> 
            match m|>Map.tryFind (Char.ToLower(x)) with
            | Some tChild -> searchDown tChild rest (t::accT) (x::accC)
            | None -> accT,accC,unmatchedCS

    let rec private buildUp ts cs prev = 
        match ts,cs with
        |  [],[] -> prev
        |  t::ts,c::cs ->
            let t2 = T(tMap t |> Map.add c prev, tStr t, tPos t)
            buildUp ts cs t2
    
    let addWithException (s:string)  ps lemma root  = 
        let lstr = match lemma with Some _ -> lemma | None -> Some(s)
        let cs = s.ToCharArray() |> Array.toList
        match root with
        | Empty -> createFromEmpty cs lstr ps
        | t ->
            let ts,matchedCS,unmatchedCS = searchDown t cs [] []
            match ts,unmatchedCS with
            | [],[] -> root
            | x::tail,[] -> //all chars matched
                match tStr x with
                | Some _ -> 
                    let existPs = tPos x
                    if existPs = ps then root
                    else
                        let newX = T(tMap x, tStr x, existPs |> Set.union ps)
                        buildUp tail matchedCS newX
                | None -> //all chars match but no string, add it
                    let baseT = T(tMap x, lstr, ps)
                    buildUp tail matchedCS baseT
            | [],cs -> //no match, branch from root
                let baseT = createFromEmpty cs lstr ps
                let mapR = (tMap root, tMap baseT) ||> Map.fold (fun acc k v -> acc |> Map.add k v)
                let newRoot = T(mapR, tStr root, tPos root)
                newRoot
            | commonAncestor::ts,cs -> //partial match
                //commonAncestor is a T with a map with [<last-char> -> <subT>,string] association
                let baseT = createFromEmpty cs lstr ps
                let coMap,coStr,coPs = tMap commonAncestor, tStr commonAncestor, tPos commonAncestor
                let childT = coMap.[matchedCS.Head]
                let childMap, childStr, childPs = tMap childT, tStr childT, tPos childT
                let baseTLastChar = (baseT |> tMap).[cs.Head]
                let childMap = childMap |> Map.add cs.Head baseTLastChar
                let newChild = T(childMap, childStr, childPs)
                let newCommon = T(coMap|>Map.add matchedCS.Head newChild,coStr,coPs)
                buildUp ts matchedCS.Tail newCommon

    let add a ps root = addWithException a ps None root

    let search (s:string) t = 
        let cs = s.ToCharArray() |> Array.toList
        let rec matchNext t unmatchedCS accT accC =
            let m = tMap t
            match unmatchedCS with
            | [] -> (t::accT),accC,unmatchedCS
            | x::rest -> 
                match m|>Map.tryFind (Char.ToLower(x)) with
                | Some tChild -> matchNext tChild rest (t::accT) (x::accC)
                | None -> accT,accC,unmatchedCS
        matchNext t cs [] []

    let find (s:string) t = search s t |> function ([],_,_) -> None | (x::_,_,_) -> x |> tStr
   
(*        
let t1 = add "test" noun Empty
t1 |> find "test"
let t2 = add "bulb" noun t1
t2 |> find "test"
t2 |> find "bulb"
let t3 = add "testing" noun t1
t3 |> find "test"
t3 |> find "testing"
let t4 = add "tumbler" noun t1
t4 |> find "test"
t4 |> find "tumbler"
let t5 = add "tesla" noun t1
t5 |> find "test"
t5 |> find "tesla"
*)