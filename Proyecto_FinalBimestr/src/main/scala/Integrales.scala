
import scala.io.StdIn.*
//------------------- Simpson 1/3 --------------------
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
  (h2 / 3) * (fc(a) + (4*(1 to n2 - 1 by 2).map(i => fc(a + i * h2)).sum) + (2*(2 to n2 - 2 by 2).map(j => fc(a+j*h2)).sum)+fc(b))
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

      // ---------------------Prints-------------------
      println("ʃ(-(x^2) + 8x - 12)dx\n"+
          s"Método de Simpson 1/3            : ${i(valInf, valSup, z => -(math.pow(z,2))+ (8*z)-12)}\n"+
          s"Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(valInf, anonimoH(valInf, valSup, 8),
                                                  8, x1 => -math.pow(x1, 2) + 8 * x1 - 12)}\n"+
          s"Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
                                                  variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)),
                                                  x1 => -math.pow(x1, 2) + 8 * x1 - 12)}\n")


    case "2" =>

      println("Ingrese el límite inferior")
      val valInf = readInt()
      println("Ingrese el límite Superior")
      val valSup = readInt()
      println("ʃ(3(x^2))dx")
      println("Método de Simpson 1/3            : " + i(valInf, valSup, d => 3 * math.pow(d, 2)))
      println("Método de Simpson 1/3 [COMPUESTO]: " + composedIntegration(valInf, anonimoH(valInf, valSup, 8), 8,
        x1 => (3 * math.pow(x1, 2))))
      println("Método de Simpson 1/3 [EXTENDIDO]: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
        variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)), x1 => (3 * math.pow(x1, 2))))


    case "3" =>
      // Simpson 1/3
      println("Ingrese el valor inferior")
      val valA = readInt()
      println("Ingrese el valor Superior")
      val valB = readInt()
      val respuesta =
      println("ʃ((x + 2x^2 - x^3 + 5x^4))dx")
      println("Método de Simpson 1/3            : " + i(valA, valB, x1 => (x1 + 2 * math.pow(x1, 2) -
        math.pow(x1, 3) + 5 * math.pow(x1, 4))))
      println("Método de Simpson 1/3 [COMPUESTO]: " + composedIntegration(valA, anonimoH(valA, valB, 8), 8,
        x1 => (x1 + 2 * math.pow(x1, 2) - math.pow(x1, 3) + 5 * math.pow(x1, 4))))
      println("Método de Simpson 1/3 [EXTENDIDO]: " + extendIntegration(valA, valB, anonimoN(valA, valB),
        variantAnonimoH(valA, valB, anonimoN(valA, valB)), x1 => (x1 + 2 * math.pow(x1, 2) - math.pow(x1, 3) + 5 * math.pow(x1, 4))))


    case "4" =>
      // Simpson 1/3

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()

      println("ʃ(2x+1)/(x^2 +1)dx")
      println("Método de Simpson 1/3            : " + i(valInf, valSup, x => ((2*x)+1)/(math.pow(x,2)+x)))
      println("Método de Simpson 1/3 [COMPUESTO]: " + composedIntegration(valInf, anonimoH(valInf, valSup, 8), 8,
        x1 => ((2 * x1 + 1) / (math.pow(x1, 2) + x1))))
      println("Método de Simpson 1/3 [EXTENDIDO]: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
        variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)), x1 => ((2 * x1 + 1) / (math.pow(x1, 2) + x1))))


    case "5" =>
      // Simpson 1/3
      val function = (x: Double) => math.pow(math.E, x) // E tiene un valor ya dado, no es calculado por Scala
      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()

      println("ʃ(e^x)dx")
      println("Método de Simpson 1/3            : " + i(valInf, valSup, x => math.pow(math.E, x)))
      println("Método de Simpson 1/3 [COMPUESTO]: " + composedIntegration(valInf, anonimoH(valInf, valSup, 8), 8,
                                                      x1 => (math.pow(math.E, x1))))

      println("Método de Simpson 1/3 [EXTENDIDO]: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
        variantAnonimoH(valInf, valSup, anonimoN(valInf, valSup)), x1 => (math.pow(math.E, x1))))


    case "6" =>
        println("Ingrese el valor inferior")
        val valInf = readInt()
        println("Ingrese el valor Superior")
        val valSup = readInt()
          println("ʃ(1/√(x-1))dx")
          println("Método de Simpson 1/3            : " + i(valInf, valSup, x => 1 / (math.sqrt(x - 1))))
          println("Método de Simpson 1/3 [COMPUESTO]: " + composedIntegration(valInf, anonimoH(valInf, valSup, 8), 8,
                                                        x1 => (1 / math.sqrt(x1 - 1))))
          println("Método de Simpson 1/3 [EXTENDIDO]: " + extendIntegration(valInf, valSup, anonimoN(valInf, valSup),
                                                          variantAnonimoH(valInf, valSup,
                                                          anonimoN(valInf, valSup)), x1 => (1 / math.sqrt(x1 - 1))))

    case "7" =>

      println("Ingrese el valor inferior")
      val valInf = readInt()
      println("Ingrese el valor Superior")
      val valSup = readInt()
      val respuesta = i(valInf, valSup, x => 1/ (1+math.pow(x, 2)))
      println("ʃ((1/(1+x^2))dx")
      println("Método de Simpson 1/3: " + respuesta)
      println("Método de Simpson 1/3 [COMPUESTO]: " + composedIntegration(valInf, anonimoH(valInf, valSup, 8), 8,
                                                      x1 => (1 / (1 + math.pow(x1, 2)))))
      println("Método de Simpson 1/3 [EXTENDIDO]: " + extendIntegration (valInf, valSup, anonimoN (valInf, valSup), variantAnonimoH
      (valInf, valSup, anonimoN (valInf, valSup) ), x1 => (1 / (1 + math.pow (x1, 2)))))
    case "8" =>
      println("+-------------------------------------------------------+\n" +
              "|  =>ʃ(-(x^2)+8x-12)dx                                  |\n" +
             s"|  Método de Simpson 1/3:           : ${i(3, 5, z => -(math.pow(z, 2)) + (8 * z) - 12)} |\n"+
             s"|  Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(3, anonimoH(3, 5, 8),
                                                       8, x1 => -math.pow(x1, 2) + 8 * x1 - 12)} |\n" +
             s"|  Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(3, 5, anonimoN(3, 5),
                                                       variantAnonimoH(3, 5, anonimoN(3, 5)),
                                                       x1 => -math.pow(x1, 2) + 8 * x1 - 12)} |\n"+
              "|  =>ʃ(3(x ^ 2))dx                                      |\n"+
             s"|  Método de Simpson 1/3            : ${i(0, 2, d => 3 * math.pow(d, 2))}\t\t\t    |\n"+
             s"|  Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(0, anonimoH(0, 2, 8), 8,
                                                       x1 => (3 * math.pow(x1, 2)))}\t\t\t    |\n" +
             s"|  Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(0, 2, anonimoN(0, 2),
                                                       variantAnonimoH(0, 2, anonimoN(0, 2)),
                                                       x1 => (3 * math.pow(x1, 2)))}\t\t\t    |\n" +
             s"|  => ʃ((x + 2x^2 - x^3 + 5x^4))dx                      |\n" +
             s"|  Método de Simpson 1/3            : ${i(-1, 1, x1 => (x1 + 2 * math.pow(x1, 2) -
                                                       math.pow(x1, 3) + 5 * math.pow(x1, 4)))} |\n" +
             s"|  Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(-1, anonimoH(-1, 1, 8), 8,
                                                       x1 => (x1 + 2 * math.pow(x1, 2) - math.pow(x1, 3) + 5 * math.pow(x1, 4)))}|\n" +
             s"|  Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(-1, 1, anonimoN(-1, 1),variantAnonimoH
                                                       (-1, 1, anonimoN(-1, 1)), x1 => (x1 + 2 * math.pow(x1, 2) -
                                                       math.pow(x1, 3) + 5 * math.pow(x1, 4)))}|\n"+
             s"|  => ʃ(2x+1)/(x^2 +1)dx                                |\n" +
             s"|  Método de Simpson 1/3            : ${i(1, 2, x => ((2*x)+1)/(math.pow(x,2)+x))}|\n" +
             s"|  Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(1, anonimoH(1, 2, 8), 8,
                                                       x1 => ((2 * x1 + 1) / (math.pow(x1, 2) + x1)))}|\n" +
             s"|  Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(1, 2, anonimoN(1, 2),
                                                       variantAnonimoH(1, 2, anonimoN(1, 2)),
                                                       x1 => ((2 * x1 + 1) / (math.pow(x1, 2) + x1)))}|\n"+
              "|  => ʃ(e^x)dx                                          |\n" +
             s"|  Método de Simpson 1/3            : ${i(0, 1, x => math.pow(math.E, x))}|\n" +
             s"|  Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(0, anonimoH(0, 1, 8), 8,
                                                       x1 => (math.pow(math.E, x1)))} |\n" +

             s"|  Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(0, 1, anonimoN(0, 1),
                                                       variantAnonimoH(0, 1, anonimoN(0, 1)),
                                                       x1 => (math.pow(math.E, x1)))}|\n" +
             s"|  => ʃ(1/√(x-1))dx                                     |\n"+
             s"|  Método de Simpson 1/3            : ${i(2, 3, x => 1 / (math.sqrt(x - 1)))}|\n" +
             s"|  Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(2, anonimoH(2, 3, 8), 8,
                                                       x1 => (1 / math.sqrt(x1 - 1)))}|\n" +
             s"|  Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(2, 3, anonimoN(2, 3),
                                                    variantAnonimoH(2, 3, anonimoN(2, 3)), x1=>(1/math.sqrt(x1-1)))}|\n"+
             s"|  => ʃ((1/(1+x^2))dx                                   |\n"+
             s"|  Método de Simpson 1/3            : ${i(0, 1, x => 1/ (1+math.pow(x, 2)))}|\n"+
             s"|  Método de Simpson 1/3 [COMPUESTO]: ${composedIntegration(0, anonimoH(0, 1, 8), 8,
                                                       x1 => (1 / (1 + math.pow(x1, 2))))}|\n" +
             s"|  Método de Simpson 1/3 [EXTENDIDO]: ${extendIntegration(0, 1, anonimoN(0, 1), variantAnonimoH
                                                       (0, 1, anonimoN(0, 1)), x1 => (1 / (1 + math.pow(x1, 2)))) }|\n"+
              "+-------------------------------------------------------+")
    case _ => println("Ingrese un valor Correcto")
  }



