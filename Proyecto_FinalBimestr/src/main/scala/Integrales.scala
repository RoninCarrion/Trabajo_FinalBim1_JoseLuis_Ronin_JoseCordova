
import scala.io.StdIn.*

@main def Main (): Unit=
  println("Integrales por las Variaciones del Método de Simpson")
  println("Ingrese:\n" +
    "[1] ʃ(-(x^2) + 8x - 12)\n" +
    "[2] ʃ(3(x^2))\n" +
    "[3] ʃ((x + 2x^2 - x^3 + 5x^4))\n")
  var option = readLine()
  option match {
    case "1" =>
      val x = (a: Double, b:Double) =>(a+b)/2
      val f_b = (y:Double)=> -v(y*y)+ (8*y)-12
      val f_a = (z:Double)=> -v(z*z)+ (8*z)-12
      val i = (valA: Double, valB:Double) => ((valB - valA) *(f_a(valA)+4*(x(valA, valB))+f_b(valB)))/6
      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup)
      println("Método de Simpson 1/3\n\t" + respuesta)
      //val ec1 : (a:Int, b:Int)=>(a to b)
    case "2" => println("February")
    case "3" => println("March")
    case _ => println("Ingrese un valor correcto")
  }
// def ec1(x: Int): Int =

// def rangoMed : (x: Int, y: Int) => (x + y)/2

