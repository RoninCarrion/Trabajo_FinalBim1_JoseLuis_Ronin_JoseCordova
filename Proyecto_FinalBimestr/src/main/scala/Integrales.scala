
import scala.io.StdIn.*

val i = (valA: Double, valB: Double, f_x: Double => Double) => ((valB - valA) * (f_x(valA) + 4 * (f_x((valA + valB) / 2)) +
  f_x(valB))) / 6

//---------------------Simpson Compuesta--------------
val anonimoH = (x: Double, y: Double, z: Double) => (y - x) / z

val composedIntegration = (a: Double, h: Double, n: Int, fc: Double => Double) => {
  (h / 3) * (1 to n / 2).toList.map(j => fc(a + ((2 * j) - 2) * h) + 4 * fc(a + ((2 * j) - 1) * h) + fc(a + (2 * j) * h)).sum
}
//--------------------Simpson Extendida---------------------------
val anonimoN = (x: Int, y: Int) => 2 * (y - x)
val variantAnonimoH = (x: Double, y: Double, n: Double) => (y - x) / n
val extendIntegration = (a: Double, b: Double, n2: Int, h2: Double, fc: Double => Double) => {
  (h2 / 3) * (fc(a) + (4*(1 to n2 - 1 by 2).map(i => fc(a + i * h2)).sum) + (2 * (2 to n2 - 2 by 2).map(j => fc(a + j * h2)).sum) + fc(b))
}


@main def Main (): Unit=
  println("Integrales por las Variaciones del Método de Simpson")
  println("Ingrese:\n" +
    "[1] ʃ(-(x^2) + 8x - 12)dx\n" +
    "[2] ʃ(3(x^2))dx\n" +
    "[3] ʃ((x + 2x^2 - x^3 + 5x^4))dx\n" +
    "[4] ʃ(2x+1)/(x^2 +1)dx\n" +
    "[5] ʃ(e^x)dx\n" +
    "[6] ʃ(1/√(x-1))dx\n" +
    "[7] ʃ((1/(1+x^2))dx\n" +
    "[8] Realizar todos los ejercicios")
  var option = readLine()
  option match {
    case "1" =>

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup, z => -(math.pow(z,2))+ (8*z)-12)

      println("Ingresa la longitud del intervalo")
      val n = scala.io.StdIn.readInt()
      if (n % 2 != 0) {
        System.exit(0)
      }
      // ---------------------Prints-------------------
      println("Método de Simpson 1/3: " + respuesta)
      println("Su resultado compuesto2 es: "+composedIntegration(valInf,anonimoH(valInf,valSup,n),n,x1 => -math.pow(x1,2)+8*x1-12))
      println("Su resultado extendidoNuevo es: " + extendIntegration(valInf,valSup,anonimoN(valInf,valSup),
        variantAnonimoH(valInf,valSup,anonimoN(valInf,valSup)),x1 => -math.pow(x1,2)+8*x1-12))


    case "2" =>

      println("Ingrese el límite inferior")
      val valInf = readInt()
      println("Ingrese el límite Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup, d => 3*math.pow(d, 2))
      println("Ingresa la longitud del intervalo")
      val n = scala.io.StdIn.readInt()
      if (n % 2 != 0) {
        System.exit(0)
      }

      println("Método de Simpson 1/3\n\t" + respuesta)
      println("Su resultado es: " + composedIntegration(valInf, anonimoH(valInf, valSup, n), n,
        x1 => (3 * math.pow(x1, 2))))
      println("Su resultado extendido es: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
        variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)), x1 => (3 * math.pow(x1, 2))))


    case "3" =>
      // Simpson 1/3
      println("Ingrese el valor inferior")
      val valA = readInt()
      println("Ingrese el valor Superior")
      val valB = readInt()
      val respuesta = i(valA, valB, x1 => (x1 + 2 * math.pow(x1, 2) - math.pow(x1, 3) + 5 * math.pow(x1, 4)))
      println("Ingresa la longitud del intervalo")
      val n = scala.io.StdIn.readInt()
      if (n % 2 != 0) {
        System.exit(0)
      }

      println("Método de Simpson 1/3\n\t" + respuesta)
      println("Su resultado  es: " + composedIntegration(valA, anonimoH(valA, valB, n), n,
        x1 => (x1 + 2 * math.pow(x1, 2) - math.pow(x1, 3) + 5 * math.pow(x1, 4))))
      println("Su resultado extendido es: " + extendIntegration(valA, valB, anonimoN(valA, valB),
        variantAnonimoH(valA, valB, anonimoN(valA, valB)), x1 => (x1 + 2 * math.pow(x1, 2) - math.pow(x1, 3) + 5 * math.pow(x1, 4))))


    case "4" =>
      // Simpson 1/3

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup, x => ((2*x)+1)/(math.pow(x,2)+x))
      println("Ingresa la longitud del intervalo")
      val n = scala.io.StdIn.readInt()
      if (n % 2 != 0) {
        System.exit(0)
      }

      println("Método de Simpson 1/3\n\t" + respuesta)
      println("Su resultado es: " + composedIntegration(valInf, anonimoH(valInf, valSup, n), n,
        x1 => ((2 * x1 + 1) / (math.pow(x1, 2) + x1))))
      println("Su resultado extendido es: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
        variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)), x1 => ((2 * x1 + 1) / (math.pow(x1, 2) + x1))))


    case "5" =>
      // Simpson 1/3
      val function = (x: Double) => math.pow(math.E, x) // E tiene un valor ya dado, no es calculado por Scala
      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup, x => math.pow(math.E, x))
      println("Ingresa la longitud del intervalo")
      val n = scala.io.StdIn.readInt()
      if (n % 2 != 0) {
        System.exit(0)
      }
      println("Método de Simpson 1/3\n\t" + respuesta)
      println("Su resultado es: " + composedIntegration(valInf, anonimoH(valInf, valSup, n), n, x1 => (math.pow(math.E, x1))))

      println("Su resultado extendido es: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
        variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)), x1 => (math.pow(math.E, x1))))


    case "6" =>

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup, x=>1/ (math.sqrt(x-1)))
      println("Ingresa la longitud del intervalo")
      val n = scala.io.StdIn.readInt()
      if (n % 2 != 0) {
        System.exit(0)
      }
      println("Método de Simpson 1/3\n\t" + respuesta)
      println("Su resultado es: " + composedIntegration(valInf, anonimoH(valInf, valSup, n), n, x1 => (1 / math.sqrt(x1 - 1))))
      println("Su resultado extendido es: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
        variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)), x1 => (1 / math.sqrt(x1 - 1))))


    case "7" =>

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup, x => 1/ (1+math.pow(x, 2)))
      println("Ingresa la longitud del intervalo")
      val n = scala.io.StdIn.readInt()
      if (n % 2 != 0) {
        System.exit(0)
      }
      println("Método de Simpson 1/3\n\t" + respuesta)
      println("Su resultado es: " + composedIntegration(valInf, anonimoH(valInf, valSup, n), n, x1 => (1 / (1 + math.pow(x1, 2)))))
      println ("Su resultado extendidoNuevo1 es: " + extendIntegration (valInf, valSup, anonimoN (valInf, valSup), variantAnonimoH
      (valInf, valSup, anonimoN (valInf, valSup) ), x1 => (1 / (1 + math.pow (x1, 2) ) ) ) )
    case _ => println("Ingrese un valor incorrecto")
  }



