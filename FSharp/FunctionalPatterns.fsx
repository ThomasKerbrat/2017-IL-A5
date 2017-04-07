// ****************************
// *                          *
// *    Functional average    *
// *                          *
// ****************************

//let average numbers =
//    let rec average' sum count numbers =
//        match numbers with
//        | [] -> float(sum) / float(count)
//        | head::tail -> average' (sum + head) (count + 1) tail
//
//    average' 0 0 numbers

type Average = Average of float * int

let aggregateAverage a1 a2 =
    let (Average(n1, c1)) = a1
    let (Average(n2, c2)) = a2

    Average((n1 * float(c1) + n2 * float(c2)) / float(c1 + c2), (c1 + c2))

[1; 2; 3; 4] |> List.reduce (fun n1 n2 -> n1 + n2)
[1; 2; 3; 4] |> List.reduce (fun n1 n2 -> (+) n1 n2)
[1; 2; 3; 4] |> List.reduce (+)

[
    Average(6.0, 3)
    Average(12.5, 8)
    Average(7.45, 4)
] |> List.reduce aggregateAverage

[
    Average(5.0, 1)
    Average(7.0, 1)
    Average(4.0, 1)
] |> List.reduce aggregateAverage

[5; 7; 4] |> List.map (fun n -> Average(float(n), 1))
          |> List.reduce aggregateAverage

let average numbers =
    numbers |> List.map (fun n -> Average(float(n), 1))
            |> List.reduce aggregateAverage
            |> (fun (Average(n, _)) -> n)

String.length "test"

let aggregateString s1 s2 =
    match String.length s1, String.length s2 with
    | l1, l2 when l1 > l2 -> s1
    | _ -> s2

[ "a"; "ab"; "abc"; "ad"; "abcd" ] |> List.reduce aggregateString

printfn "%s" "test"
printfn "%i" 25
printfn "%f" 25.0

type StringLength = StringLength of string * int

let longestString strings =
    let longestString' s1 s2 =
        match s1, s2 with
        | StringLength(_, l1), StringLength(_, l2) when l1 > l2 -> s1
        | _ -> s2

    strings |> List.map (fun s -> StringLength(s, String.length s))
            |> List.reduce longestString'
            |> (fun (StringLength(s, _)) -> s)

longestString [ "a"; "ab"; "abc"; "ad" ]

// ******************************
// *                            *
// *    Segment partitioning    *
// *                            *
// ******************************

type Segment = Segment of int * int

let createSegment start length = Segment(start, length)

let segmentOverlap s1 s2 =
    match s1, s2 with
    | Segment(start1, length1), Segment(start2, length2) when start1 > (start2 + length2) || start2 > (start1 + length1) -> false
    | _ -> true

let a = createSegment 1 2
let b = createSegment 2 2
let c = createSegment 5 1
let d = createSegment 7 3
let e = createSegment 8 1

printfn "a overlaps b: %b" (segmentOverlap a b)
printfn "b overlaps a: %b" (segmentOverlap b a)
printfn "a overlaps c: %b" (segmentOverlap a c)
printfn "d overlaps e: %b" (segmentOverlap d e)
printfn "e overlaps d: %b" (segmentOverlap e d)

type SegmentPartition = SegmentPartition of List<Segment> * Segment

let partitionOverlap p1 p2 =
    let (SegmentPartition(_, s1)) = p1
    let (SegmentPartition(_, s2)) = p2
    segmentOverlap s1 s2

let mergeSegment s1 s2 =
    let (Segment(start1, length1)) = s1
    let (Segment(start2, length2)) = s2
    let end1 = start1 + length1
    let end2 = start2 + length2
    let start = System.Math.Min(start1, start2)
    let length = System.Math.Max(end1, end2) - start
    createSegment start length

let mergePartition partitions =
    let aggregatePartition p1 p2 =
        let (SegmentPartition(s1, r1)) = p1
        let (SegmentPartition(s2, r2)) = p2
        SegmentPartition((List.concat [s1; s2]), (mergeSegment r1 r2))

    partitions |> List.reduce aggregatePartition

let aggregate l1 l2 =
    let add partitions partition =
        let toMerge, other = partitions |> List.partition (partitionOverlap partition)
        let merged = mergePartition (partition::toMerge)
        merged::other

    List.concat [l1; l2] |> List.fold add []

let partition segments =
    segments |> List.map (fun s -> [SegmentPartition([s], s)])
             |> List.reduce aggregate
             |> List.map (fun (SegmentPartition(partition, _)) -> partition)

partition [a; b; c; d; e]

// *****************************
// *                           *
// *    Railway programming    *
// *                           *
// *****************************

open System.IO

type Result<'TSucess, 'TFailure> =
| Success of 'TSucess
| Failure of 'TFailure

type User = User of string * string

let validateUser (User(firstName, lastName)) =
    match String.length firstName, String.length lastName with
    | 0, _ -> Failure("The first name must be not empty")
    | _, 0 -> Failure("The last name must be not empty")
    | _ -> Success(User(firstName, lastName))

let storeUser filePath (User(firstName, lastName)) =
    File.AppendAllText(filePath, sprintf "%s;%s\n" firstName lastName)

let mapDeadEnd f v =
    f v
    v

let map f r =
    match r with
    | Success(s) -> Success(f s)
    | Failure(r) -> Failure(r)

let printUser (User(firstName, lastName)) =
    printfn "First name: %s - last name: %s" firstName lastName

let printFailure s =
    printfn "%s" s

let split success failure r =
    match r with
    | Success(s) -> Success(success s)
    | Failure(f) -> Failure(failure f)

let (=>) v f = v |> (map (mapDeadEnd f))

let (!!) f = mapDeadEnd f

let (<=>) = split

let (>=<) f1 f2 = !!f1 <=> !!f2

User("FirstName5", "LastName5") |> validateUser
                                => storeUser "D:\\users.csv"
                                |> (printUser >=< printFailure)
