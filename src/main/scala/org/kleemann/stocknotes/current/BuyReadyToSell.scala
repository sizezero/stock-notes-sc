package org.kleemann.stocknotes.current

import org.kleemann.stocknotes.{Fraction}
import org.kleemann.stocknotes.stock.{Buy}

/**
 * This is a buy that is ready to be matched with sells to take some of the shares.
 *
 * @param buy The buy associated with these shares to sell.
 * @param unsold unsold shares at the multiple Fraction.one
 */
final private[current] case class BuyReadyToSell(buy: Buy, unsold: Double)

private[current] object BuyReadyToSell {
  // default has all shares unsold
  def apply(buy: Buy) = new BuyReadyToSell(buy, buy.shares.atMult(Fraction.one))
}


