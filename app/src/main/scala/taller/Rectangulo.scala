package taller

/** Punto 1. Un rectángulo dado por su base y su altura, ambas enteras. Los
  * objetos no cambian: rotar y escalar devuelven un rectángulo nuevo.
  *
  * Tal como está compila y las pruebas quedan en rojo.
  */
class Rectangulo(b: Int, h: Int) {

  // Selectoras: la base y la altura con que se construyó el rectángulo.
  def base: Int = b // Completar

  def altura: Int = h // Completar

  def area: Int = b * h // Completar

  def perimetro: Int = 2 * (b + h) // Completar

  def esCuadrado: Boolean = b == h // Completar

  // El rectángulo con base y altura intercambiadas.
  def rotar: Rectangulo = new Rectangulo(h, b) // Completar

  // El rectángulo con los dos lados multiplicados por k.
  def escalar(k: Int): Rectangulo = new Rectangulo(b * k, h * k) // Completar

  // Si este rectángulo entra dentro de otro, tal cual o rotado.
  def cabeEn(otro: Rectangulo): Boolean = 
    (b <= otro.base && h <= otro.altura) || 
    (h <= otro.base && b <= otro.altura)
   // Completar

  // El de mayor área entre este y otro; con áreas iguales, este.
  def elMayor(otro: Rectangulo): Rectangulo = 
    if (area >= otro.area) this else otro // Completar

  // La forma "3x4": base, la letra x y altura.
  override def toString: String = base + "x" + altura // Completar
}
