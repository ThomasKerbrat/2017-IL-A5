let unitPrice = 20
let items = 5
let total = 5 * 12

let isNegative = fun x -> x < 0
isNegative total

(fun x -> x < 0) total

let f = fun x ->
    match x with
    | 20 -> -20
    | 30 -> 60
    | _ -> x

printfn "%d" (f 20)
printfn "%d" (f 30)
printfn "%d" (f 40)

(fun n ->
    match n with
    | _ when n < 0 -> -n
    | _ when n = 1 -> 2
    | _ -> n) 1

// Exercise 1

let isMultipleOf5 =
    fun x -> x%5 = 0

let display =
    fun x ->
        match isMultipleOf5 x with
        | true -> printfn "%i multiple de 5" x
        | false -> printfn "%i pas multiple de 5" x

let rec processUntil100 =
    fun x ->
        match x <= 100 with
        | true -> 
            display x
            processUntil100 (x+1)
        | false -> () 

processUntil100 1
