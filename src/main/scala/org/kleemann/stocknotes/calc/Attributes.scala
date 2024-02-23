package org.kleemann.stocknotes.calc

import org.kleemann.stocknotes.stock.Currency

/**
 * This is the list of all possible attributes both parsed and generated.
 * Any attribute may be missing.
 *
 * @param income annual income in dollars
 * @param revenue annual revenue in dollars
 * @param shares share count, hopefully fully diluted
 * @param eps earnings per share in dollars
 * @param pe price earnings multiple
 * @param marketCap total market capitalization in dollars
 * @param price price per share in dollars
 * @param margins net margins: income/revenue
 * @param dividend annual dividend per share
 * @param dividendYield annual dividend yield
 * @param payoutRatio dividend / income
 * @param help not really an attribute, if defined, indicates that help should be displayed
 */
private final case class Attributes(
        income:        Option[Currency] = None,
        revenue:       Option[Currency] = None,
        shares:        Option[Int]      = None,
        eps:           Option[Currency] = None,
        pe:            Option[Double]   = None,
        marketCap:     Option[Currency] = None,
        price:         Option[Currency] = None,
        margins:       Option[Double]   = None,
        dividend:      Option[Currency] = None,
        dividendYield: Option[Double]   = None,
        payoutRatio:   Option[Double]   = None,
        help:          Option[Boolean]  = None
    )
