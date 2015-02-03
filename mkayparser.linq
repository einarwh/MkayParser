<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference Version="4.0.0">FSharp.Core</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open System
open FParsec

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let str = pstring
let ws = spaces

let stringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

let mself = pchar '.'

type MItem =
 | MSelf
 | MAnd
 | MOr
 | MPlus
 | MMinus
 | MMult
 | MDiv
 | MEq
 | MGt
 | MLt
 | MGteq
 | MLteq
 | MIdent of string
 | MString of string
 | MInt of int
 | MBool of bool
 | MList of MItem list

let mbool = (stringReturn "true"  (MBool true))
           <|> (stringReturn "false" (MBool false))

let mlogic = (stringReturn "&&" MAnd) <|> (stringReturn "||" MOr)

let marith = 
  (stringReturn "+" MPlus) <|> 
  (stringReturn "-" MMinus) <|>
  (stringReturn "*" MMult) <|>
  (stringReturn "/" MDiv)

let mcomp = 
  (stringReturn "==" MEq) <|>
  (stringReturn "==" MGt) <|>
  (stringReturn "==" MLt) <|>
  (stringReturn "==" MGteq) <|>
  (stringReturn "==" MLteq)

let mint = pint32 |>> MInt

let mstr = stringLiteral |>> MString

let mvalue, mvalueRef = createParserForwardedToRef<MItem, unit>()

let listOfThings item f =
    between (str "(") (str ")")
            (ws >>. (many (item .>> ws)) |>> f)

let mlist = listOfThings mvalue MList

do mvalueRef := choice [mlist
                        mstr
                        mint
                        mlogic
                        marith
                        mcomp
                        mbool]

test mself "."
test mself "foo"

test mbool "true"
test mbool "phoney"

test mint "123"
test mint "abc"

test mstr "\"Quux\""
test mstr "Quux"

test mlist "(true)"
test mlist "(true 123 \"Quux\")"
test mlist "(true >= 123 * == (\"Quux\"))"

test mlogic "&&"
test mlogic "||"
test mlogic "123"

test marith "+"
test marith "0"
test marith "/"

let rec meval (exp : MItem) =
  match exp with
   | MBool b -> b
   | MList lst -> 
     match lst with
      | MAnd :: tl -> tl |> List.forall meval
      | MOr :: tl -> tl |> List.exists meval
	  | MEq :: MInt n1 :: MInt n2 :: [] -> n1 = n2
	  | MLt :: MInt n1 :: MInt n2 :: [] -> n1 < n2
	  | MGt :: MInt n1 :: MInt n2 :: [] -> n1 > n2
	  | MLteq :: MInt n1 :: MInt n2 :: [] -> n1 <= n2
	  | MGteq :: MInt n1 :: MInt n2 :: [] -> n1 >= n2
      | _ -> false
   | _ -> false

let ast = run mlist "(&& true true)"

let doeval s = 
  match run mlist s with
   | Success(result, _, _) -> meval result
   | Failure(errorMsg, _, _) -> false
   
let res0 = doeval "(|| false true)"
let res1 = doeval "(&& false true)"
let res2 = doeval "(&& true true)"
let res3 = doeval "(== 5 5)"
let res4 = doeval "(== 5 6)"
   
Console.WriteLine res0
Console.WriteLine res1
Console.WriteLine res2
Console.WriteLine res3
Console.WriteLine res4

