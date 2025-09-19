open System

let Func x = 3.0 ** x

let factorial n =
    let rec loop acc = function
        | 0 -> acc
        | n -> loop (acc * n) (n - 1)
    loop 1 n

let dumbTaylor x eps =
    let ln3 = Math.Log(3.0)
    let rec calculateTerm n =
        (Math.Pow(ln3, float n) / float (factorial n)) * Math.Pow(x, float n)
    
    let rec sumTerms n acc =
        let term = calculateTerm n
        if abs term < eps then 
            (acc, n)
        else 
            sumTerms (n + 1) (acc + term)
    
    sumTerms 0 0.0

let smartTaylor x eps =
    let ln3 = Math.Log(3.0)
    let rec sumTerms n prevTerm sum =
        let term = if n = 0 then 1.0 else prevTerm * ln3 * x / float n
        if abs term < eps then 
            (sum, n)
        else 
            sumTerms (n + 1) term (sum + term)
    
    sumTerms 0 0.0 0.0
let generatePoint a b step =
    seq { a .. step .. b }
let printTable a b step eps =
    printfn "x\t\tBuiltin\t\tSmart Taylor\t# terms\tDumb Taylor\t# terms"
    for x in generatePoint a b step do
        let builtin = Func x
        let (smartVal, smartTerms) = smartTaylor x eps
        let (dumbVal, dumbTerms) = dumbTaylor x eps
        printfn "%.2f\t\t%.6f\t%.6f\t%d\t%.6f\t%d" x builtin smartVal smartTerms dumbVal dumbTerms

let a = 0.0
let b = 1.0
let step = 0.1
let eps = 1e-6
printTable a b step eps