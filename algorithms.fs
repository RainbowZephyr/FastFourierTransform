module algorithms

open System
open System.Numerics
open System.Threading.Tasks

let complexE = Complex(Math.E, 0.0)

let complexPI = Complex(Math.PI, 0.0)

let complexIndex index = Complex((float index), 0.0)

let splitList list =
    list
    |> List.mapi (fun i x -> (i % 2 = 0, x))
    |> List.partition fst
    |> fun (odd, even) -> List.map snd odd, List.map snd even

let fourierMultiply (head: Complex) (k: int) (n: int) (length: int) : Complex =
    (head
     * (Complex.Pow(
         complexE,
         (-2.0
          * Math.PI
          * Complex.ImaginaryOne
          * float k
          * float n)
         / Complex(float (length), 0.0)
     )))


let fft (signal: List<double>) : List<Complex> =
    let rec fft_helper (complexSignal: List<Complex>) (index: int) (acc: List<Complex>) =
        match complexSignal with
        | head :: tail when index < complexSignal.Length ->
            fft_helper
                complexSignal
                (index + 1)
                ((List.fold
                    (fun fs1 fs2 -> fs1 + fs2)
                    Complex.Zero
                    (List.mapi (fun i fx -> fourierMultiply fx index i complexSignal.Length) complexSignal))
                 :: acc)
        | _ -> acc

    let complexSignal =
        (signal |> List.map (fun n -> Complex(n, 0.0)))

    let result = fft_helper complexSignal 0 []
    List.rev result

let fftRadix2 (signal: List<double>) : List<Complex> =
    let rec fft_helper
        (complexSignal: List<Complex>)
        (complexEven: List<Complex>)
        (complexOdd: List<Complex>)
        (index: int)
        (acc: List<Complex>)
        =
        match complexSignal with
        | head :: tail when index < complexSignal.Length ->
            fft_helper
                complexSignal
                complexEven
                complexOdd
                (index + 1)
                ((List.fold
                    (fun fs1 fs2 -> fs1 + fs2)
                    Complex.Zero
                    (List.mapi (fun i fx -> fourierMultiply fx index (i * 2) (complexSignal.Length)) complexEven))
                 + (List.fold
                     (fun fs1 fs2 -> fs1 + fs2)
                     Complex.Zero
                     (List.mapi (fun i fx -> fourierMultiply fx index (i * 2 + 1) (complexSignal.Length)) complexOdd))
                 :: acc)
        | _ -> acc

    let complexSignal =
        (signal |> List.map (fun n -> Complex(n, 0.0)))

    let split = splitList complexSignal


    let result =
        fft_helper complexSignal (fst split) (snd split) 0 []

    List.rev result




[<EntryPoint>]
let main argv =
    let z =
        [ 1.0
          1.0
          1.0
          1.0
          0.0
          0.0
          0.0
          0.0 ]

    let r : List<Complex> = fftRadix2 z

    printfn "RESULT ARRAY\n %A" r

    0