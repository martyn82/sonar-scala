
class ScalaFile3 {
  private val b = 0

  {
    while (b < 10) {
      b = b + 1
    }
  }

  // measure complexity on this
  def foo = {
    var a = 10
    while (a > 0) {
      a = a - 1
    }

    val b = false

    try {
      b = true
    } catch {
      case e: Throwable => "error"
    }

    val c = if (b) {
      44
    } else if (!b) {
      67
    } else {
      50
    }
  }
}
