package taller

/** Punto 4. Un número complejo r + i·i, con r la parte real e i la
  * imaginaria. Los objetos no cambian: cada operación devuelve uno nuevo.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Complejos(val r: Double, val i: Double) {

  def +(otro: Complejos): Complejos = new Complejos(r + otro.r, i + otro.i) // Completar

  def -(otro: Complejos): Complejos = new Complejos(r - otro.r, i - otro.i) // Completar

  def *(otro: Complejos): Complejos = new Complejos(r * otro.r - i * otro.i, r * otro.i + i * otro.r) // Completar

  def /(otro: Complejos): Complejos = {
    val denominador = otro.r * otro.r + otro.i * otro.i
    
    new Complejos(
      (r * otro.r + i * otro.i) / denominador,
      (i * otro.r - r * otro.i) / denominador
    )
  } // Completar

  // "a + bi" con las dos partes redondeadas a tres decimales; si la parte
  // imaginaria es negativa, "a - bi".
  override def toString: String = {
    val parteReal = f"$r%.3f"
    val parteImaginaria = f"$i%.3f"
    if (parteImaginaria.startsWith("-")) {
      s"$parteReal ${parteImaginaria.substring(1)}i"
    } else {
      s"$parteReal + $parteImaginariai"
    }
  } // Completar
}
