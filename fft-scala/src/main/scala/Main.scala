import org.apache.commons.math3.complex.Complex
import scalaz.Memo

import scala.collection.mutable
import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.HashMap

object Main {
    val complexPI = new Complex(Math.PI)
    val complexE = new Complex(Math.E)
    var mutableMap = new mutable.HashMap[Int, Complex]()

    def splitList(list: List[Complex]): List[List[Complex]] = {
        val evenOdd = list.zipWithIndex.map(x => (x._2 % 2 == 0, x._1)).partition(z => {z._1})

        List(evenOdd._1.map(t => t._2), evenOdd._2.map(t => t._2))
    }

    def expo(k: Int, n: Int, length: Int): Complex = {
        complexE.pow((new Complex(-2.0)).multiply(complexPI).multiply(Complex.I).multiply(new Complex(k)).multiply(new Complex(n)).divide(new Complex(length)))
    }

    def fourierMultiply(head: Complex, k: Int, n: Int, length: Int): Complex = {
        head.multiply(expo(k, n, length))
    }

    def fourier(index: Int, signalLength: Int, complexSignal: List[Complex]): Complex = {
        if (mutableMap.contains(index)) {
            mutableMap(index)
        } else {
            val r: Complex = complexSignal.par
              .zipWithIndex
              .map((z: (Complex, Int)) => fourierMultiply(z._1, index, z._2, signalLength))
              .reduce((fs1, fs2) => fs1.add(fs2))
            mutableMap.put(index, r)
            r
        }
    }

    //    (List.fold
    //    (fun fs1 fs2 -> fs1 + fs2)
    //      Complex.Zero
    //    (List.mapi(fun i fx -> fourierMultiply fx index i signalLength) complexSignal)
    //    )


    def expoE(k: Int, length: Int): Complex = {
        complexE.pow((new Complex(-2.0)).multiply(complexPI).multiply(Complex.I).multiply(new Complex(k)).divide(new Complex(length)))
    }

    def fft(signal: List[Double]) : List[Complex] = {
        val complexSignal = signal.map(t=> new Complex(t))
        fftHelper(complexSignal, 0, List[Complex]())
    }

    def fftHelper(complexSignal: List[Complex], index: Int, acc: List[Complex]): List[Complex] ={
        if(index < complexSignal.length) {
            fftHelper(complexSignal,
                index+1,
                complexSignal.zipWithIndex.map(t=> fourierMultiply(t._1, index, t._2, complexSignal.length)).reduce((fs1, fs2)=> fs1.add(fs2))::acc)
        } else {
            acc.reverse
        }
    }

    def fftRadix2(signal: List[Double]): List[Complex] = {
        val complexList = signal.par.map(d => new Complex(d)).toList
        val evenOdd = splitList(complexList)
        //        println("SPLITTED "+evenOdd)

        fftRadix2Helper(signal.length, (evenOdd.head: List[Complex]), (evenOdd(1): List[Complex]), 0, (expoE(0, signal.length)), List[Complex](), List[Complex]())

    }

    def fftRadix2Helper(signalLength: Int, complexEven: List[Complex], complexOdd: List[Complex], index: Int, e: Complex, accl: List[Complex], accr: List[Complex]): List[Complex] = {
//        println("I "+index + " E "+e)
        if (index < signalLength / 2) {
            fftRadix2Helper(signalLength,
              complexEven,
              complexOdd,
              (index + 1),
              expoE(index + 1, signalLength),
              fourier(index, signalLength / 2, complexEven)
                .add(e.multiply(fourier(index, signalLength / 2, complexOdd))) :: accl,
              fourier(index, signalLength / 2, complexEven)
                .subtract(e.multiply(fourier(index, signalLength / 2, complexOdd)))::accr)
        } else {
            accl.reverse ::: accr.reverse
        }

    }


    def main(args: Array[String]) = {
//        println("Hello, world")
//        var list = List[Double](1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0)
//        val z = splitList(list.map(t=> new Complex(t)))

//        println(fftRadix2(list))
        //        println(splitList(list))
        var list = Seq.fill(100_000)(scala.util.Random.nextDouble).toList;
        val start = System.nanoTime()
        fftRadix2(list)
        val end = System.nanoTime()
        println("Total Time Millis " + (end-start)/1_000_000)
        println("Total Time Seconds " + (end-start)/1_000_000_000)

    }
}
