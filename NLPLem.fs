module NLPLem
open System
open NLPWTree

let (|Ends|_|) stem xs  = 
    let rec loop xs ys =
        match xs,ys with
        | [],[] -> Some(xs)
        | [],_ -> None
        | _, [] -> Some(xs)
        | a::xs,b::ys when a = b || Char.ToLower(a) = Char.ToLower(b) -> loop xs ys
        | _ -> None
    loop xs stem

let (|Starts|_|) = (|Ends|_|) //same processing but semantically different use

(*
let prepWords =
    [
    "to"
    "at"
    "of"
    "on"
    "off"
    "in"
    "out"
    "up"
    "down"
    "from"
    "with"
    "into"
    "for"
    "about"
    "between"
    ]

let ps = prepWords |> List.map (fun p -> p.ToCharArray() |> Array.toList |> List.rev)
*)

let prepositions =  
   [['o'; 't']; ['t'; 'a']; ['f'; 'o']; ['n'; 'o']; ['f'; 'f'; 'o']; ['n'; 'i'];
   ['t'; 'u'; 'o']; ['p'; 'u']; ['n'; 'w'; 'o'; 'd']; ['m'; 'o'; 'r'; 'f'];
   ['h'; 't'; 'i'; 'w']; ['o'; 't'; 'n'; 'i']; ['r'; 'o'; 'f'];
   ['t'; 'u'; 'o'; 'b'; 'a']; ['n'; 'e'; 'e'; 'w'; 't'; 'e'; 'b']]

let nounEndings = 
    [
        ['s']                   ,[]; 
        ['s'; 'e'; 's']         ,[['s']]
        ['s'; 'e'; 'x']         ,[['x']]
        ['s'; 'e'; 'z']         ,[['z']]
        ['s'; 'e'; 'h'; 'c']    ,[['c';'h']]
        ['s'; 'e'; 'h'; 's']    ,[['s';'h']]
        ['n'; 'e'; 'm']         ,[['m';'a';'n']]
        ['s'; 'e'; 'i']         ,[['y']]
    ]

let verbEndings =
    [
       ['s']            ,[]
       ['s'; 'e'; 'i']  ,[['y']]
       ['s'; 'e']       ,[['e'];[]]
       ['d'; 'e']       ,[['e'];[]]
       ['g'; 'n'; 'i']  ,[['e'];[]]
    ]

let adjEndings = 
    [
        ['r'; 'e']      , [[];['e']]
        ['t'; 's'; 'e'] , [[];['e']]
    ]

let findBase cs =
    [nounEndings; verbEndings; adjEndings] 
    |> List.tryPick (List.tryPick (fun (e,vars) -> match cs with Ends e rest -> Some (rest,vars) | _ -> None)) 

let findVariants cs =
    [nounEndings; verbEndings; adjEndings]
    |> List.collect (List.choose (fun (e,vars) -> match cs with Ends e rest -> Some (rest,vars) | _ -> None))

let findVariant altEndings t = 
    let variant =
        match altEndings with
        | [] -> t |> tStr |> Option.map (fun s -> s,tPos t)
        | es ->
            es |> List.tryPick (fun ending ->
                let rec loop echars t =
                    match echars with
                    | [] -> 
                        match t with
                        | T(_,Some(s),pos) -> Some(s,pos)
                        | _ -> None
                    | x::rest -> 
                        let m = tMap t
                        match m |> Map.tryFind x with
                        | Some t -> loop rest t
                        | None -> None
                loop ending t)
    variant

let isPunctuation c = Char.IsPunctuation c
let isWhitespace c = Char.IsWhiteSpace c

type L = L of string*Set<Pos> | NS of string | R of string | Lnk of string | H of string

let (<|>) a b = fun x -> a x || b x

let toStr cs = String(cs|>List.rev|>List.toArray)

let lemmatize (s:string) root =
    let cs = s.ToCharArray() |> Array.toList
    seq {

        let rec matchNext t unmatchedCS prevTs matchedCS =
            match unmatchedCS with
            | [] -> //end of input
                match prevTs with
                | [] -> Seq.empty
                | T(_x,Some s,ps)::_ -> seq{yield L(s,ps)}
                | _ -> Seq.empty
            | x::rest -> 
                let m = tMap t
                let isWs = isWhitespace x
                match isWs,prevTs,unmatchedCS with
                | true,  x::y, _ -> seq{yield! tryMatchColocation t unmatchedCS (t,unmatchedCS,prevTs,matchedCS)}
                | true,  []  , _   -> seq{yield! skipWhitespace rest}
                | _   ,  []  , Starts ['@'] (y::rest) when isWhitespace y |> not -> seq {yield! matchRef unmatchedCS}
                | _   ,  []  , Starts ['#'] (y::rest) when isWhitespace y |> not -> seq {yield! matchRef unmatchedCS}
                | _   ,  []  , Starts ['h';'t';'t';'p';':';'/';'/'] (y::rest) when isWhitespace y |> not -> seq {yield! matchRef unmatchedCS}
                | _,_,_ ->
                    match m.TryFind (Char.ToLower(x)) with
                    | Some tChild -> matchNext tChild rest (t::prevTs) (x::matchedCS)
                    | None -> 
                        let isWsOrP = (isWhitespace<|>isPunctuation) x
                        match isWsOrP,t with
                        | true,  T(_,Some s,ps) -> seq{yield L(s,ps); yield! skipWhitespace rest}
                        | false, T(_,Some s,ps) -> seq{yield L(s,ps); yield! skipToWhiteSpace unmatchedCS} //try to match the rest of the word from beginning
                        | true,  _ -> seq{yield! tryBaseWord matchedCS rest ((matchedCS,prevTs)||>List.zip)}
                        | false, _ -> seq{yield! skipToWSAndTryBase matchedCS unmatchedCS ((matchedCS,prevTs)||>List.zip)}

        and tryMatchColocation t unmatchedCS backtrack  =
            let doBacktrack() =
                let t,rest,prevTs,matchedCS = backtrack
                match t,matchedCS,prevTs with
                | T(_,Some s,ps),_,_ -> seq{yield L(s,ps); yield! matchNext root rest [] []}
                | _ -> seq{yield! tryBaseWord matchedCS rest ((matchedCS,prevTs)||>List.zip)}
            match unmatchedCS with
            | [] -> doBacktrack()
            | x::rest ->
                let m = tMap t
                match m.TryFind (Char.ToLower(x)) with
                | Some tChild -> tryMatchColocation tChild rest backtrack
                | None -> 
                    match t with
                    | T(_,Some s,ps) -> seq{yield L(s,ps); yield! matchNext root unmatchedCS [] []}
                    | _ -> doBacktrack()

        and tryBaseWord delmitedChars unmatchedCS matchedTs =
            match delmitedChars with
            | [] -> seq{yield! skipWhitespace unmatchedCS}
            | _ ->
                findVariants delmitedChars 
                |> List.filter (fun (xs,ys) -> List.isEmpty xs |> not)
                |> List.tryPick (fun (x::_,endings) ->
                    matchedTs |> List.tryPick (fun (backChar,t) -> if backChar=x then Some(t |> tChild x,endings) else None)
                    |> Option.bind (fun (t,endings) -> t |> findVariant endings))
                |> function
                | Some sp -> seq{yield L sp; yield! skipWhitespace unmatchedCS}
                | None -> seq{yield NS(toStr delmitedChars); yield! skipWhitespace unmatchedCS}

        and skipToWhiteSpace cs =
            match cs with
            | [] -> Seq.empty
            | x::rest -> 
                if isWhitespace x |> not then skipToWhiteSpace rest
                else seq{yield! skipWhitespace rest}

        and skipWhitespace cs =
            match cs with
            | [] -> Seq.empty
            | x::rest -> 
                if isWhitespace x then skipWhitespace rest
                else seq{yield! matchNext root cs [] []}

        and skipToWSAndTryBase csToMatch unmatchedCS prevTs =
            match unmatchedCS with
            | [] -> seq{yield! tryBaseWord csToMatch [] prevTs}
            | x::rest when x |> (isWhitespace<|>isPunctuation) |> not  -> skipToWSAndTryBase (x::csToMatch) rest prevTs
            | _ -> seq{yield! tryBaseWord csToMatch unmatchedCS prevTs}

        and matchRef unmatchedCS = 
            let f = match unmatchedCS with [] -> NS | '@'::_ -> R | '#'::_ -> H | _ -> Lnk
            let rec loop unmatchedCS acc =
                match unmatchedCS with
                | [] ->  acc,unmatchedCS
                | x::rest when isWhitespace x -> acc,unmatchedCS
                | x::rest -> loop rest (x::acc)
            let acc,rest = loop unmatchedCS []
            seq {yield f (acc|>toStr); yield! matchNext root rest [] []}

        yield! matchNext root cs [] []}
   