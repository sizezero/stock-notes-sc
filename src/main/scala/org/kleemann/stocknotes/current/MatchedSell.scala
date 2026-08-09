package org.kleemann.stocknotes.current

import org.kleemann.stocknotes.{Currency, Fraction}
import org.kleemann.stocknotes.stock.{Shares}
import org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}
import scala.annotation.tailrec

/**
 * This represents a sale of a block of stock.
 * It is matched up with perhaps multiple buy blocks of stock.
 * 
 * @param sell The Trade Sell that is associated with this sale. All the shares of this Sell trade are sold thus they are all matched with buys.
 * @param net gross - sell commission, the cash you get when you sell
 * @param capitalGain net - sell commissions - buy commissions proportional to the shares sold from the buy batch
 * @param mbs A list of Matched buys that add up to this sale from oldest to newest.
 */
final case class MatchedSell(
  sell: Sell,
  net: Currency,
  capitalGain: Currency,
  mbs: List[MatchedBuy])

object MatchedSell {
  /**
    * Associates all the shares of the given sell with some of the given buys.
    * We consume from the head of the buys since those are the oldest.
    * 
    * @param sell the shares to sell
    * @param buys the list of buys from which we can consume
    * @return The MatchedSell and BuyReadyToSells that have yet to be matched with sells.
    */
  def sell2MatchedSell(sell: Sell, buys: Vector[BuyReadyToSell]): (MatchedSell, Vector[BuyReadyToSell]) = {

    // Unfortunately it's not possible to do this calculation with integer share values plus multiples.
    // We need to use lossy Doubles. This is highlighted in the test case "parseCompanyDateRange fractional matching"
    // which caused the old code to blow up.

    // all double calculations are done in multiple Fraction.one
    // when we produce final share counts, they are converted to the sell multiple
    def sharesToDouble(s: Shares): Double = s.atMult(Fraction.one)
    def doubleToShares(d: Double): Shares = Shares((d * sell.shares.multiple.toDouble).round.toInt, sell.shares.multiple)

    // Recursively walk through the BuyReadyToSell list consuming buys, and accumulate our matched buy list
    // until we have satisfied all shares in the sell.
    // Note: both buy and sell shares are converted to Double at multiple Fraction.one.
    @tailrec
    def assignBuys(toBuys: Vector[BuyReadyToSell], toSell: Double, mbs: List[MatchedBuy]): (Vector[BuyReadyToSell], List[MatchedBuy]) = {
      if (Shares.equals(toSell, 0.0)) (toBuys, mbs) // shares to sell are zero, we are done
      else {
        val b: BuyReadyToSell = toBuys.head
        if (Shares.equals(b.unsold, toSell)) { // b.unsold == toSell
          // the current buy perfectly completes the sell
          val mb = MatchedBuy(sell, b.buy, doubleToShares(toSell))
          (toBuys.tail, mb :: mbs)
        } else if (b.unsold < toSell) {
          // we have used up the current buy, but the sell is not done
          val mb = MatchedBuy(sell, b.buy, doubleToShares(b.unsold))
          // our sold shares could be rounded up or down so use this value instead of b.unsold to subtract from toSell
          assignBuys(toBuys.tail, toSell-b.unsold, mb :: mbs)
        } else { // b.unsold > toSell
          // only some of the buy shares are needed to satisfy the sell
          val mb = MatchedBuy(sell, b.buy, doubleToShares(toSell))
          val reducedShares: Double = b.unsold - toSell
          // sell is completed and the next buy is only partially completed
          (toBuys.updated(0, BuyReadyToSell(b.buy, reducedShares)), mb :: mbs)
        }
      }
    }
    val (toBuys: Vector[BuyReadyToSell], mbsReversed: List[MatchedBuy]) =
      assignBuys(buys, sharesToDouble(sell.shares), Nil)

    val mbs = mbsReversed.reverse
    val net = sell.gross - mbs.foldLeft(Currency.zero){ _ + _.proportionalCost }
    val capitalGain = net - sell.commission - mbs.foldLeft(Currency.zero){ _ + _.proportionalBuyCommission }
    (MatchedSell(sell, net.truncate, capitalGain.truncate, mbs), toBuys)
  }

}