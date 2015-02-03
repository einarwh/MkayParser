<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <NuGetReference Version="4.0.0">FSharp.Core</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

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
  (stringReturn ">" MGt) <|>
  (stringReturn "<" MLt) <|>
  (stringReturn ">=" MGteq) <|>
  (stringReturn "<=" MLteq)

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

type RVal = RString of string | RInt of int | RBool of bool | RNaught

let rec boolVals (vs : RVal list) =
  match vs with
   | [] -> []
   | h::t -> match h with 
              | RBool bv -> bv :: boolVals t
			  | _ -> raise (System.Exception("Type error: Expected RBool"))

let rec meval (exp : MItem) : RVal =
  match exp with
   | MBool b -> RBool b
   | MInt n -> RInt n
   | MString s -> RString s
   | MList lst -> 
     match lst with
      | MAnd :: tl -> 
		let res = List.map meval tl |> boolVals |> List.forall (fun x -> x)
		RBool res
      | MOr :: tl -> 
	    let res = List.map meval tl |> boolVals |> List.exists (fun x -> x)
		RBool res
	  | MEq :: x :: y :: [] -> RBool (meval x = meval y)
	  | MLt :: x :: y :: [] -> RBool (meval x < meval y)
	  | MGt :: x :: y :: [] -> RBool (meval x > meval y)
	  | MLteq :: x :: y :: [] -> RBool (meval x <= meval y)
	  | MGteq :: x :: y :: [] -> RBool (meval x >= meval y)
	  | MPlus :: x :: y :: [] -> 
	    let x1 = meval x
		let y1 = meval y
		match (x1, y1) with 
		 | (RInt nx, RInt ny) -> RInt (nx + ny)
		 | (RString sx, RString sy) -> RString (sx + sy)
		 | _ -> raise (System.Exception("Type error: Not addable."))
	  | _ -> RNaught
   | _ -> RNaught

let ast = run mlist "(&& true true)"

let doeval s = 
  match run mlist s with
   | Success(result, _, _) -> meval result
   | Failure(errorMsg, _, _) -> RBool false

let output s = 
  Console.Write (s + " => ")
  match doeval s with 
   | RBool b -> 
     Console.WriteLine b
   | RInt n -> 
     Console.WriteLine n
   | RString s ->
     Console.WriteLine s
   | _ -> Console.WriteLine "Huh"
   
output "(|| false true)"
output "(&& false true)"
output "(&& true true)"
output "(== 5 5)"
output "(== 5 6)"
output "(< 5 6)"
output "(|| (== 5 6) (< 5 6))"
output "(+ 5 6)"
output "(== \"foo\" \"foo\")"
output "(+ \"foo\" \"bar\")"
