package taller

/** Punto 3. Un número racional x/y guardado en su forma normalizada:
  * numerador y denominador sin factores comunes, el signo en el numerador y
  * el denominador siempre positivo. El cero se guarda como 0/1.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Racional(x: Int, y: Int) {

  // Precondición del constructor: el denominador no es cero.
  require(y != 0)

  // El máximo común divisor de dos enteros no negativos.
  private def mcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else mcd(b, a % b)
  }

  // Valores normalizados
  private val divisor = mcd(Math.abs(x), Math.abs(y))

  private val n: Int = {
    if (x == 0) {
      0
    } else if (y < 0) {
      -Math.abs(x) / divisor
    } else {
      Math.abs(x) * (if (x < 0) -1 else 1) / divisor
    }
  }

  private val d: Int = {
    if (x == 0) {
      1
    } else {
      Math.abs(y) / divisor
    }
  }

  // Numerador y denominador ya normalizados.
  def numer: Int = {
    if (x == 0) {
      0
    } else {
      val signo = if (y < 0) -1 else 1
      val divisor = mcd(Math.abs(x), Math.abs(y))
      signo * Math.abs(x) / divisor
    }
  }

  def denom: Int = {
    if (x == 0) {
      1
    } else {
      Math.abs(y) / mcd(Math.abs(x), Math.abs(y))
    }
  }

  def +(r: Racional): Racional = {
    new Racional(
      numer * r.denom + r.numer * denom,
      denom * r.denom
    )
  }

  def -(r: Racional): Racional = {
    new Racional(
      numer * r.denom - r.numer * denom,
      denom * r.denom
    )
  }

  def *(r: Racional): Racional = {
    new Racional(
      numer * r.numer,
      denom * r.denom
    )
  }

  def /(r: Racional): Racional = {
    require(r.numer != 0)

    new Racional(
      numer * r.denom,
      denom * r.numer
    )
  }

  // Si los dos racionales representan el mismo número.
  def ==(r: Racional): Boolean = {
    numer == r.numer && denom == r.denom
  }

  def <(r: Racional): Boolean = {
    numer * r.denom < r.numer * denom
  }

  // El mayor de los dos.
  def max(r: Racional): Racional = {
    if (this < r) r
    else this
  }

  // "n/d", o solo "n" cuando el denominador es 1.
  override def toString: String = {
    if (denom == 1) numer.toString
    else s"$numer/$denom"
  }
}