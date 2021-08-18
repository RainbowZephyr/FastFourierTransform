module algorithms

open System
open System.Numerics
open System.Threading.Tasks

let complexE = Complex(Math.E, 0.0)

let complexPI = Complex(Math.PI, 0.0)

let complexIndex index = Complex((float index), 0.0)

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

let from whom = sprintf "from %s" whom

let fft (signal: List<double>) : List<Complex> =
    let rec fft_helper (complexSignal: List<Complex>) (index: int) (acc: List<Complex>) =
        match complexSignal with
        | [] -> acc
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

    let lazy_signal =
        (signal |> List.map (fun n -> Complex(n, 0.0)))

    let z = fft_helper lazy_signal 0 []
    z



[<EntryPoint>]
let main argv =
//    let rand = System.Random()
//    let z = [1.0;1.0;1.0;1.0;0.0;0.0;0.0;0.0]
////    printfn "%A" z
////    printfn "============================="
//    let r:List<Complex> = fft z
//    printfn "RESULT ARRAY %A" r
    0
