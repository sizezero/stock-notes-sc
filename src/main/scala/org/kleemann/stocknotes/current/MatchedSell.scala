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

    // this is some tiny share value at multiple one that we are comfortable that share counts will never get below
    // we use this to see if share counts are equal or at zero since we can't use equality on a Double
    val quantum: Double = 0.0001

    // all double calculations and all conversion to Shares are done with the sell multiple
    val m = sell.shares.multiple

    // a tempory structure to build our eventual matched buys
    //case class IncompleteMatchedBuy(buy: Buy, sold: Shares) // sold shares are in the Sell multiple

    // Recursively walk through the BuyReadyToSell list consuming buys, and accumulate our incomplete matched buy list
    // until we have satisfied all shares in the sell.
    // Note: both buy and sell shares are converted to Double at multiple Fraction.one.
    @tailrec
    def assignBuys(toBuys: Vector[BuyReadyToSell], toSell: Double, mbs: List[MatchedBuy]): (Vector[BuyReadyToSell], List[MatchedBuy]) = {
      if (toSell <= quantum) (toBuys, mbs) // shares to sell are zero, we are done
      else {
        val b: BuyReadyToSell = toBuys.head
        if (Math.abs(b.unsold - toSell) < quantum) { // b.unsold == toSell
          // the current buy perfectly completes the sell
          val s = Shares((toSell * m.toDouble).round.toInt, m)
          (toBuys.tail, MatchedBuy(sell, b.buy, s) :: mbs)
        } else if (b.unsold < toSell) {
          // we have used up the current buy, but the sell is not done
          val s = Shares((b.unsold * m.toDouble).round.toInt, m)
          assignBuys(toBuys.tail, toSell-b.unsold, MatchedBuy(sell, b.buy, s) :: mbs)
        } else { // b.unsold > toSell
          // only some of the buy shares are needed to satisfy the sell
          val s = Shares((toSell * m.toDouble).round.toInt, m)
          val reducedShares: Double = b.unsold - toSell
          // sell is completed and the next buy is only partially completed
          (toBuys.updated(0, BuyReadyToSell(b.buy, reducedShares)), MatchedBuy(sell, b.buy, s) :: mbs)
        }
      }
    }
    val (toBuys: Vector[BuyReadyToSell], mbsReversed: List[MatchedBuy]) =
      assignBuys(buys, sell.shares.atMult(Fraction.one), Nil)

    val mbs = mbsReversed.reverse
    val net = sell.gross - mbs.foldLeft(Currency.zero){ _ + _.proportionalCost }
    val capitalGain = net - sell.commission - mbs.foldLeft(Currency.zero){ _ + _.proportionalBuyCommission }
    (MatchedSell(sell, net.truncate, capitalGain.truncate, mbs), toBuys)
  }

}