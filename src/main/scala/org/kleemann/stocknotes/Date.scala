package org.kleemann.stocknotes

final case class Date private(year: Int, month: Int, day: Int) extends Ordered[Date] {
  override def compare(that: Date): Int = {
    if (this.year != that.year) this.year - that.year
    else if (this.month != that.month) this.month - that.month
    else this.day - that.day
  }
}

object Date {

  private val number2month: Map[Int, String] = Map(
    1 -> "January", 2 -> "February", 3 -> "March", 4 -> "April",
    5 -> "May", 6 -> "June", 7 -> "July", 8 -> "August", 9 -> "September", 10 -> "October",
    11 -> "November", 12 -> "December"
  )

  /** lower case three letter abbreviation of month to one based number of month
    * 
    */
  private val month2number: Map[String, Int] =
    number2month.keys.map{ (number: Int) => {
        val abbreviation: String = number2month(number).toLowerCase().take(3)
        abbreviation -> number
    }}.toMap

  /** The zero index exists so that we can use one based month indeces */
  private val daysPerMonth: Vector[Int] = Vector(
    0,  //no month
    31, //jan
    28, //feb
    31, //mar
    30, //apr
    31, //may
    30, //jun
    31, //jul
    31, //aug
    30, //sep
    31, //oct
    30, //nov
    31  //dec
  )

  private val cumulativeDaysPerMonth: Vector[Int] = {
    // start with the iterative solution
    val a = scala.collection.mutable.ArrayBuffer[Int]()
    a += daysPerMonth(0)
    for (i <- 2 to 12) {
        a += ( a.last + daysPerMonth(i) )
    }
    a.toVector
  }

  def apply(year: Int, month: Int, day: Int): Option[Date] =
    if (year<1900 || year>3000) None
    else if (month<1 || month>12) None
    else if (day<1 || day>daysPerMonth(month)) None
    else Some(new Date(year,month,day))

  /** returns the current date
   * Note: this is not functional as multiple calls will return different values
   */
  def today: Date = {
    import java.util.Calendar
    var dT = Calendar.getInstance();
        
    val year  = dT.get(Calendar.YEAR)
    val month = dT.get(Calendar.MONTH) + 1
    val day   = dT.get(Calendar.DATE)
    new Date(year, month, day)
  }

  private val datePattern = """^\s*([A-Za-z]+)\s+(\d{1,2}),\s*(\d{4})\s*$""".r

  def parse(line: String): Option[Date] = {
    line match {
        case datePattern(month,day,year) => {
          month2number.get(month.toLowerCase()).flatMap { m =>
            // the strings coming out of the RE are guarateed to be integers
            Date(year.toInt, m, day.toInt)
          }
        }
        case _ => None
    }
  }
}