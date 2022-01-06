package com.ilscipio.scipio.ce.base.util.fin;

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Objects;

/**
 * A generic price amount and currency type class.
 * <p>SCIPIO: 2.1.0: Added for help with Solr fields.</p>
 */
public class StdPrice implements Serializable {
    public static final StdPrice NULL = new StdPrice(null, null);
    public static final int DEFAULT_SCALE = 2;
    public static final RoundingMode DEFAULT_ROUNDING = RoundingMode.HALF_UP;
    public static final int DETAILED_SCALE = 3;

    protected final BigDecimal amount;
    protected final String currencyUomId;

    public StdPrice(BigDecimal amount, String currencyUomId) {
        this.amount = amount;
        this.currencyUomId = currencyUomId;
    }

    /**
     * Makes a new price instance from amount and currencyUomId, with explicit scaling.
     * @param scaling A scale value for rounding amount decimals, -1 to auto-determine defaults or null for no scaling
     */
    public static StdPrice from(BigDecimal amount, String currencyUomId, Integer scaling) {
        if (scaling != null) {
            amount = amount.setScale(scaling >= 0 ? scaling : DEFAULT_SCALE, DEFAULT_ROUNDING);
        }
        return new StdPrice(amount, currencyUomId);
    }

    /**
     * Makes a new price instance from amount and currencyUomId, with default scaling.
     */
    public static StdPrice from(BigDecimal amount, String currencyUomId) {
        return from(amount, currencyUomId, -1);
    }

    /**
     * Splits an amount and currency price value in the form "1.23,USD" based on the Solr currency format to a price.
     * @param splitPrice A split price string in the form "1.23,USD", or "[amount],[currencyUomId]
     * @param scaling A scale value for rounding amount decimals, -1 for defaults (auto-determine) or null for no scaling
     */
    public static StdPrice from(String splitPrice, Integer scaling) {
        int commaIndex = splitPrice.indexOf(',');
        if (commaIndex < 0) {
            return NULL;
        }
        BigDecimal amount = new BigDecimal(splitPrice.substring(0, commaIndex));
        if (scaling != null) {
            amount = amount.setScale(scaling >= 0 ? scaling : DEFAULT_SCALE, DEFAULT_ROUNDING);
        }
        return new StdPrice(amount, splitPrice.substring(commaIndex + 1));
    }

    /**
     * Splits an amount and currency price value in the form "1.23,USD" based on the Solr currency format to a price.
     * @param splitPrice A split price string in the form "1.23,USD", or "[amount],[currencyUomId]
     */
    public static StdPrice from(String splitPrice) {
        return from(splitPrice, -1);
    }

    public BigDecimal getAmount() {
        return amount;
    }

    public String getCurrencyUomId() {
        return currencyUomId;
    }

    @Override
    public String toString() {
        return toSplitString();
    }

    public String toSplitString() {
        return getAmount() + "," + getCurrencyUomId();
    }
}
