
import scala.io.StdIn.*

@main def Main (): Unit=
  println("Integrales por las Variaciones del Método de Simpson")
  println("Ingrese:\n" +
    "[1] ʃ(-(x^2) + 8x - 12)dx\n" +
    "[2] ʃ(3(x^2))dx\n" +
    "[3] ʃ((x + 2x^2 - x^3 + 5x^4))dx\n" +
    "[4] ʃ(2x+1)/(x^2 +1)dx\n" +
    "[5] ʃ(e^x)dx\n" +
    "[6] ʃ(1/√(x-1))dx\n" +
    "[7] ʃ((1/(1+x^2))dx\n")
  var option = readLine()
  option match {
    case "1" =>
      // val x = (a: Double, b:Double) =>(a+b)/2
      val function= (y:Double)=> -(y*y)+ (8*y)-12
      // val f_a = (z:Double)=> -(z*z)+ (8*z)-12
      val i = (valA: Double, valB:Double) => ((valB - valA) *(function(valA)+4*(function((valA + valB)/2))+
        function(valB)))/6
      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup)
      println("Método de Simpson 1/3\n\t" + respuesta)
      //val ec1 : (a:Int, b:Int)=>(a to b)

      // Simpson 1/3 Extendida
      // val anonimoH =(x:Double, y: Double, n: Double)=> (x-y)/n
      // val xj = (valA: Double, valH: Double) => valA + 1* valH
      // val simExt = (vaA: Double, vaH: Double) => (1 to (10000/2)).map(k=> )
      // ------------------------------------------------------------------------------------------------
    case "2" =>
      val function = (d: Double) => 3*math.pow(d, 2)
      val i = (valA: Double, valB: Double) => ((valB - valA) * (function(valA) + 4 * function((valA+valB)/2) +
        (function(valB)))) / 6
      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup)

      println("Método de Simpson 1/3\n\t" + respuesta)

    case "3" => println("March")
      // Simpson 1/3
      val function = (x: Double) => x *(1 + (2 * x) - (math.pow(x,2)) + (5 * math.pow(x, 3)))
      val i = (valA: Double, valB: Double) => ((valB - valA) * (function(valA) + 4 * function((valA + valB) / 2) +
        (function(valB)))) / 6

      println("Ingrese el valor inferior")
      val valA = readInt()
      println("Ingrese el valor Superior")
      val valB = readInt()
      val respuesta = i(valA, valB)
      println("Método de Simpson 1/3\n\t" + respuesta)
       println("respuesta: " + (valB - valA))
       println("f(a):"+ function(valA))
      println("f(b):"+ function(valB))
      println("f(x):"+ function((valA + valB) / 2))

    case "4" =>
      // Simpson 1/3
      val function = (x:Double)=> ((2*x)+1)/(math.pow(x,2)+x)
      val i = (valA: Double, valB: Double) => ((valB - valA) * (function(valA) + 4 * function((valA + valB) / 2) +
        (function(valB)))) / 6

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup)
      println("Método de Simpson 1/3\n\t" + respuesta)
    case "5" =>
      // Simpson 1/3
      val function = (x: Double) => math.pow(math.E, x) // E tiene un valor ya dado, no es calculado por Scala
      // val e = math.pow((1+(1/1000000)),1000000)
      val i = (valA: Double, valB: Double) => ((valB - valA) * (function(valA) + 4 * function((valA + valB) / 2) +
        (function(valB)))) / 6
      // println("Ingrese el valor para euler")
      // val valEuler = readInt()
      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup)
      println("Método de Simpson 1/3\n\t" + respuesta)
      // println("euler:"+ e(1000000))

    case "6" =>
      // Simpson 1/3
      val function = (x: Double) => 1/ (math.sqrt(x-1))
      val i = (valA: Double, valB: Double) => ((valB - valA) * (function(valA) + 4 * function((valA + valB) / 2) +
        (function(valB)))) / 6

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup)
      println("Método de Simpson 1/3\n\t" + respuesta)

    case "7" =>
      // Simpson 1/3
      val function = (x: Double) => 1/ (1+math.pow(x, 2))
      val i = (valA: Double, valB: Double) => ((valB - valA) * (function(valA) + 4 * function((valA + valB) / 2) +
        (function(valB)))) / 6

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup)
      println("Método de Simpson 1/3\n\t" + respuesta)
    case _ => println("Ingrese un valor correcto")
  }


