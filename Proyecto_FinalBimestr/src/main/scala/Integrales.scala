
import scala.io.StdIn.*

@main def Main (): Unit=
  println("Integrales por las Variaciones del Método de Simpson")
  println("Ingrese:\n" +
    "[1] ʃ(-(x^2) + 8x - 12)\n" +
    "[2] ʃ(3(x^2))\n" +
    "[3] ʃ((x + 2x^2 - x^3 + 5x^4))\n")
  var option = readLine()
  option match {
    case "1" => println("Ingrese el valor inferior")
      var valInf = readInt()
      println("Ingrese el valor Superior")
      var valSup = readInt()
      //val ec1 : (a:Int, b:Int)=>(a to b)
    case "2" => println("February")
    case "3" => println("March")
    case _ => println("Ingrese un valor correcto")
  }
// def ec1(x: Int): Int =

// def rangoMed : (x: Int, y: Int) => (x + y)/2

