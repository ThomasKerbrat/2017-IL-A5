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

// Exercise 2

let fizz = fun n -> n%3 = 0
let buzz = fun n -> n%5 = 0
let fizzBuzz = fun n -> fizz n && buzz n

let displayFizzbuzz = 
    fun n ->
        match n with
        | x when fizzBuzz x -> printfn "FizzBuzz"
        | x when fizz x -> printfn "Fizz"
        | x when buzz x -> printfn "Buzz"
        | _ -> printfn "%d" n

let rec fb100 =
    fun n ->
        match n with
        | x when x <= 100 ->
            displayFizzbuzz x
            fb100 (x+1)
        | _ -> ()
        

fb100 1

// Exercise 3

let fb =
    fun n ->
        let rec fb' =
            fun range ->
                let n, max = range
                match n with
                | x when x <= max ->
                    displayFizzbuzz x
                    fb' (n + 1, max)
                | _ -> ()
        fb'(1, n)

fb 30

// Exercise 4

let fb2 =
    let rec fb2' = fun start -> fun max ->
        match start with
        | x when x <= max ->
            displayFizzbuzz x
            fb2' (start + 1) max
        | _ -> ()
    fb2' 1

fb2 40

// Lists

type List<'T> =
| Empty
| Item of 'T * List<'T>

let add = fun item -> fun list ->
    Item(item, list)

let l1 = add "test1" Empty
let l2 = add "test2" l1

let rec displayList = fun list ->
    match list with
    | Empty -> ()
    | Item(head, tail) ->
        printfn "%O" head
        displayList tail

printfn "%A" l2

let rec addLast = fun item -> fun list ->
    match list with
    | Empty -> add item Empty
    | Item(head, tail) ->
        let l = addLast item tail
        add head l

let l3 = addLast "test3" l2

type Option<'T> =
| Value of 'T
| None

let rec getAt = fun list -> fun i ->
    match list, i with
    | Empty, _ -> None
    | Item(head, _), 0 -> Value head
    | Item(_, tail), _ -> getAt tail (i - 1)

let rec removeAt = fun list -> fun i ->
    match list, i with
    | Empty, _ -> None
    | Item(_, tail), 0 -> Value tail
    | Item(head, tail), _ ->
        let l = removeAt tail (i - 1)
        match l with
        | None -> None
        | Value(v) -> Value(Item(head, v))

let l4 = removeAt l3 1