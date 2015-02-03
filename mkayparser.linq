<Query Kind="FSharpProgram">
  <NuGetReference>FParsec</NuGetReference>
  <Namespace>FParsec</Namespace>
</Query>

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
 | MIdent of string
 | MString of string
 | MInt of int
 | MBool of bool
 | MList of MItem list

let mbool = (stringReturn "true"  (MBool true))
           <|> (stringReturn "false" (MBool false))

let mlogic = (stringReturn "&&" MAnd) <|> (stringReturn "||" MOr)

let mint = pint32 |>> MInt

let mstr = stringLiteral |>> MString

let mvalue, mvalueRef = createParserForwardedToRef<MItem, unit>()

let listBetweenStrings item f =
    between (str "(") (str ")")
            (ws >>. (many (item .>> ws)) |>> f)

let mlist   = listBetweenStrings mvalue MList

do mvalueRef := choice [mlist
                        mstr
                        mint
                        mlogic
                        mbool]

test mself "."
test mself "foo"

test mbool "true"
test mbool "phoney"

test mint "123"
test mint "abc"

test mstr "\"Quux\""
test mstr "Quux"

test mlist "(true 123 \"Quux\")"
test mlist "(true 123 (\"Quux\"))"

test mlogic "&&"
test mlogic "||"
test mlogic "123"