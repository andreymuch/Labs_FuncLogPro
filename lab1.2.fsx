open System

let dichotomy (f: float -> float) (a: float) (b: float) (eps: float) =
    let rec loop a b =
        let c = (a + b) / 2.0
        if abs(f c) < eps || abs(b - a) < eps then c
        else if f a * f c < 0.0 then loop a c
        else loop c b
    loop a b
let iteration (phi: float -> float) (x0: float) (eps: float) =
    let rec loop x =
        let x1 = phi x
        if abs(x1 - x) < eps then x1
        else loop x1
    loop x0

let newton (f: float -> float) (f' : float -> float) (x0: float) (eps: float) =
    let rec loop x =
        let x1 = x - f(x) / f'(x)
        if abs(x1 - x) < eps then x1
        else loop x1
    loop x0
let f12 x = log x - x + 1.8
let f12' x = 1.0/x - 1.0
let phi12 x = log x + 1.8

let f13 x = x * tan x - 1.0/3.0
let f13' x = tan x + x / (cos x * cos x)
let phi13 x = atan (1.0 / (3.0 * x))

let f14 x = tan(x/2.0) - 1.0/tan(x/2.0) + x
let f14' x = 0.5 / (cos(x/2.0) * cos(x/2.0)) +
             0.5 / (sin(x/2.0) * sin(x/2.0)) +
             1.0
let phi14 x = -tan(x/2.0) + 1.0/tan(x/2.0)
let eps = 1e-6 // Точность

let results =
    [|
        "ln(x) - x + 1.8 = 0", 
            dichotomy f12 2.0 3.0 eps, 
            iteration phi12 2.5 eps, 
            newton f12 f12' 2.5 eps

        "x * tan(x) - 1/3 = 0", 
            dichotomy f13 0.2 1.0 eps, 
            iteration phi13 0.5 eps, 
            newton f13 f13' 0.5 eps

        "tan(x/2) - cot(x/2) + x = 0", 
            dichotomy f14 1.0 2.0 eps, 
            iteration phi14 1.5 eps, 
            newton f14 f14' 1.5 eps
    |]

printfn "%-35s | %-12s | %-12s | %-12s" "Уравнение" "Дихотомия" "Итерации" "Ньютон"
printfn "--------------------------------------------------------------------------------------------------"
for (eq, d, i, n) in results do
    printfn "%-35s | %-12f | %-12f | %-12f" eq d i n
