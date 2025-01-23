package org.kleemann.stocknotes.current

import org.kleemann.stocknotes.{Currency}
import org.kleemann.stocknotes.stock.{Shares}
import org.kleemann.stocknotes.stock.{Buy, Sell}


/**
 * This represents a block of bought shares that are now the result of a possibly larger sale.
 * Note: not all of the shares of the buy block may have been sold.
 *
 * @param buy the Buy Trade
 * @param sold the number of shares sold from this Buy Trade: sold <= buy.shares. This is in the Sell multiple.
 * @param price this is the price of the shares in the Sell multiple. This may have sub-penny precision.
 * @param proportionalCost the cost proportional to the sold shares. This may have sub-penny precision.
 * @param proportionalBuyCommission the buy commission proportional to the sold shares. This may have sub-penny precision.
 * @param proportionalSellCommission the sell commision proportional to the sold shares. This may have sub-penny precision.
 * @param ltcg true if the shares were purchased at least a year before the sale.
 * @param annualYield The annual yield from cost+proportionalBuyCommission to the gross-proportionalSellCommission
 */
final case class MatchedBuy(
  buy: Buy,
  sold: Shares,
  price: Currency,
  proportionalCost: Currency,
  proportionalBuyCommission: Currency,
  proportionalSellCommission: Currency,
  ltcg: Boolean,
  annualYield: Double)

object MatchedBuy {
  /**
    * Calculate some values needed by MatchedBuy
    *
    * @param sell the Sell Trade
    * @param buy one of the (possibly many) Buy Trades matched with the sell
    * @param sold the number shares from the buy used to satisfy the sell. A single buy may be satisfied with multiple sells.
    * @return
    */
  def apply(sell: Sell, buy: Buy, sold: Shares) = {

    // we need to convert the buy price to the sell multiple
    val price = buy.price.priceMultipleAdjust(buy.shares.multiple, sell.shares.multiple)

    // the proportion of sold shares in this sell batch vs total shares in this sell batch
    // If all shares in the buy equals all the sold shares then this is 1.0
    val proportionBuy: Double = sold.atMult(sell.shares.multiple) / buy.shares.atMult(sell.shares.multiple)

    // the proportion of sold shares in this sell batch vs the total shares in the sell
    val proportionSell: Double = sold.atMult(sell.shares.multiple) / sell.shares.atMult(sell.shares.multiple)

    val proportionalSellCommission: Currency = Currency.fromDouble(sell.commission.toDouble * proportionSell)
    val proportionalBuyCommission:  Currency = Currency.fromDouble(buy .commission.toDouble * proportionBuy)

    // we can use non-multiple share count since this is the shares at the time of the buy
    val totalBuyCost = buy.shares.shares * buy.price.toDouble
    val proportionalBuyCost: Currency = Currency.fromDouble(totalBuyCost * proportionBuy)

    val diff: Double = sell.date.decimalYear - buy.date.decimalYear
    val ltcg: Boolean = diff >= 1.0

    // include commissions in annual yield calc
    val proportionalSellGross: Currency = Currency.fromDouble(sell.gross.toDouble * proportionSell)
    val ay: Double = annualYield(
        proportionalBuyCost   + proportionalBuyCommission, 
        proportionalSellGross - proportionalSellCommission, 
        diff)

    new MatchedBuy(buy, sold, price, proportionalBuyCost, proportionalBuyCommission, proportionalSellCommission, ltcg, ay)
  }

  private[current] def annualYield(start: Currency, end: Currency, decimalYearsDifference: Double): Double =
    // there are several undefined cases where we just return zero
    if (end==start || decimalYearsDifference<0.001 || start==Currency.zero) return 0.0
    else Math.pow(end.toDouble / start.toDouble, 1.0 / decimalYearsDifference) - 1.0

}