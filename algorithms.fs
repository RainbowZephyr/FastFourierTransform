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

let expo (k: int) (n: int) (length: int) : Complex =
    Complex.Pow(
        complexE,
        (-2.0
         * Math.PI
         * Complex.ImaginaryOne
         * float k
         * float n)
        / Complex(float (length), 0.0)
    )

let expoE (k: int) (length: int) : Complex =
    Complex.Pow(
        complexE,
        (-2.0 * Math.PI * Complex.ImaginaryOne * float k)
        / Complex(float (length), 0.0)
    )

let fourierMultiply (head: Complex) (k: int) (n: int) (length: int) : Complex = (head * (expo k n length))

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

let memoize fn =
    let cache =
        new System.Collections.Generic.Dictionary<_, _>()

    (fun x ->
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let v = fn (x)
            cache.Add(x, v)
            v)

let fourier (index: int) (signalLength: int) (complexSignal: List<Complex>) : Complex =
    (List.fold
        (fun fs1 fs2 -> fs1 + fs2)
        Complex.Zero
        (List.mapi (fun i fx -> fourierMultiply fx index i signalLength) complexSignal))

let memFourier = memoize fourier

let fftRadix2 (signal: List<double>) : List<Complex> =
    let rec fft_helper
        (signalLength: int)
        (complexEven: List<Complex>)
        (complexOdd: List<Complex>)
        (index: int)
        (e: Complex)
        (accl: List<Complex>)
        (accr: List<Complex>)
        =
        match complexEven with
        | head :: tail when index < (signalLength / 2) ->
            fft_helper
                signalLength
                complexEven
                complexOdd
                (index + 1)
                (expoE (index + 1) signalLength)
                ((memFourier index (signalLength / 2) complexEven) + e * (memFourier index (signalLength / 2) complexOdd) :: accl)
                ((memFourier index (signalLength / 2) complexEven) - e * (memFourier index (signalLength / 2) complexOdd) :: accr)
        | _ -> (List.rev accl) @ (List.rev accr)

    let complexSignal =
        (signal |> List.map (fun n -> Complex(n, 0.0)))

    let split = splitList complexSignal


    let result =
        fft_helper complexSignal.Length (fst split) (snd split) 0 (expoE 0 complexSignal.Length) [] []

    result




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

    //    let r : List<Complex> = fftRadix2 z

    //    printfn "RESULT ARRAY\n %A" r

    0