module algorithms

open System
open System.Numerics
open FSharp.Collections.ParallelSeq

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

//Standard fourier transform
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

// FFT Radix2 implementation
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
                ((memFourier index (signalLength / 2) complexEven)
                 + e
                   * (memFourier index (signalLength / 2) complexOdd)
                 :: accl)
                ((memFourier index (signalLength / 2) complexEven)
                 - e
                   * (memFourier index (signalLength / 2) complexOdd)
                 :: accr)
        | _ -> (List.rev accl) @ (List.rev accr)

    let complexSignal =
        (signal |> List.map (fun n -> Complex(n, 0.0)))

    let split = splitList complexSignal


    let result =
        fft_helper complexSignal.Length (fst split) (snd split) 0 (expoE 0 complexSignal.Length) [] []

    result

let parallelFourier (index: int) (signalLength: int) (complexSignal: Complex[]) : Complex =
    (PSeq.fold
        (fun fs1 fs2 -> fs1 + fs2)
        Complex.Zero
        (Array.Parallel.mapi (fun i fx -> fourierMultiply fx index i signalLength) complexSignal))

let memParallelFourier = memoize parallelFourier

// Parallel FFT Radix2 Implementation
let parallelFFTRadix2 (signal: List<double>) : List<Complex> =
    let rec fft_helper
        (signalLength: int)
        (complexEven: Complex[])
        (complexOdd: Complex[])
        (index: int)
        (e: Complex)
        (accl: List<Complex>)
        (accr: List<Complex>)
        =
        match complexEven with
        | complexEven when index < (signalLength / 2) ->
            fft_helper
                signalLength
                complexEven
                complexOdd
                (index + 1)
                (expoE (index + 1) signalLength)
                ((memParallelFourier index (signalLength / 2) complexEven)
                 + e
                   * (memParallelFourier index (signalLength / 2) complexOdd)
                 :: accl)
                ((memParallelFourier index (signalLength / 2) complexEven)
                 - e
                   * (memParallelFourier index (signalLength / 2) complexOdd)
                 :: accr)
        | _ -> (List.rev accl) @ (List.rev accr)

    let complexSignal =
        (signal |> List.toArray |> Array.Parallel.map (fun n -> Complex(n, 0.0)) |> Array.toList)

    let split = splitList complexSignal


    let result =
        fft_helper complexSignal.Length (List.toArray (fst split)) (List.toArray (snd split)) 0 (expoE 0 complexSignal.Length) [] []

    result

// FHT Implementation
let hartley (x: double) (n: int) (k: int) (length: int) : double =
    x
    * (Math.Cos(
        (2.0 * Math.PI) / (float length)
        * (float n)
        * (float k)
       )
       + Math.Sin(
           (2.0 * Math.PI) / (float length)
           * (float n)
           * (float k)
       ));;

let fht (signal: List<double>) : List<double> =
    let rec fht_helper (signalLength: int) (realSignal: List<double>) (index: int) (acc: List<double>) =
        match realSignal with
        | head :: tail when index < realSignal.Length ->
            fht_helper
                signalLength
                realSignal
                (index + 1)
                ((List.fold
                    (fun fs1 fs2 -> fs1 + fs2)
                    0.0
                    (List.mapi (fun i fx -> hartley fx i index signalLength) realSignal))
                 :: acc)
        | _ -> acc

    let result = fht_helper signal.Length signal 0 []
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

    let r : List<_> = fht z

    printfn "RESULT ARRAY\n %A" r

    0