package org.kleemann.stocknotes

final case class Date private(year: Int, month: Int, day: Int) extends Ordered[Date] {
  override def compare(that: Date): Int = {
    if (this.year != that.year) this.year - that.year
    else if (this.month != that.month) this.month - that.month
    else this.day - that.day
  }

  // TODO: don't know if I want both these representation available
  //override def toString(): String = f"$year%04d/$month%02d/$day%02d"
  override def toString(): String = f"${Date.number2month(month).take(3)} ${day}%d, ${year}%04d"

 /**
   * Represents the date as a decimal number where the year is the whole number part and the 
   * month and day are the fraction that is proportional to how many days we are through the years.
   * This is useful for calculating the difference between dates in years.
   * 
   * @return Years are whole numbers while month and days combine into the fractional part
   */
  def decimalYear: Double = 
    year + (Date.cumulativeDaysPerMonth(month) + day) / 365.0

}

object Date {

  private val number2month: Map[Int, String] = Map(
    1 -> "January", 2 -> "February", 3 -> "March", 4 -> "April",
    5 -> "May", 6 -> "June", 7 -> "July", 8 -> "August", 9 -> "September", 10 -> "October",
    11 -> "November", 12 -> "December"
  )

  /** full lowercase month and lower case three letter abbreviation of month to one based number of month
   * 
   */
  private val month2number: Map[String, Int] =
    number2month.keys.flatMap{ (number: Int) => {
      val fullMonth = number2month(number).toLowerCase()
      val abbreviation: String = fullMonth.take(3)
      List(fullMonth -> number, abbreviation -> number)
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

  // each month index has the sum of all previous months
  // needed by decimalYear
  // note: scan() produces a 365 element at index 14 that we don't need
  private val cumulativeDaysPerMonth: Vector[Int] = daysPerMonth.scan(0){ _ + _ }.dropRight(1)

  def apply(year: Int, month: Int, day: Int): Option[Date] =
    if (year<1900 || year>3000) None
    else if (month<1 || month>12) None
    else if (day<1 || day>daysPerMonth(month)) None
    else Some(new Date(year,month,day))

  def earliest(year: Int): Option[Date] = Date(year,  1,  1)
  def latest  (year: Int): Option[Date] = Date(year, 12, 31)

  // It's a little inconsistent to have these return Dates while the above returns Options
  // but it's accurate and type checking will catch problems
  val earliest: Date = earliest(1900).get
  val latest:   Date = latest  (3000).get

  /** returns the current date
   * Note: this is not a pure function as multiple calls will return different values
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

  /**
    * Parses dates of the format: "MMM DD, YYYY" where MMM is Apr, Jan, etc.
    *
    * @param line
    * @return
    */
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