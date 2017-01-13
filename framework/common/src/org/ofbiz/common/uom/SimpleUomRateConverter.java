package org.ofbiz.common.uom;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;

/**
 * SCIPIO: Caching Uom simple rate converter that caches 
 * simple multiplied rates for frequent reuse within a thread.
 * If errors occur, they are printed to log only once so the system doesn't overload.
 * ONLY WORKS with multiplicative measures, such as currencies.
 * <p>
 * All methods return null if inputs are null or errors occur.
 * All methods are transaction-safe.
 */
public class SimpleUomRateConverter {

    private static final BigDecimal RATE_FAIL = BigDecimal.ZERO.subtract(BigDecimal.ONE);
    
    //private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    
    // map of keys in the format:
    // USD::EUR
    // that map the conversion multiplier from one to the next
    // Special negative values indicate previous failed lookups 
    private Map<String, BigDecimal> rateMap = new HashMap<>();

    public SimpleUomRateConverter(Delegator delegator, LocalDispatcher dispatcher) {
        //this.delegator = delegator;
        this.dispatcher = dispatcher;
    }
    
    public BigDecimal getConvertRate(String uomId, String uomIdTo) {
        if (uomId == null || uomIdTo == null) return null;
        final String key = uomId + "::" + uomIdTo;
        BigDecimal rate = rateMap.get(key);
        
        if (rate != null) {
            if (rate.compareTo(BigDecimal.ZERO) < 0) {
                return null;
            } else {
                return rate;
            }
        } else {
            rate = lookupConvertRate(uomId, uomIdTo);
            if (rate != null) {
                rateMap.put(key, rate);
            } else {
                rateMap.put(key, RATE_FAIL);
            }
            return rate;
        }
    }
    
    protected BigDecimal lookupConvertRate(String uomId, String uomIdTo) {
        return UomWorker.convertUomSafe(BigDecimal.ONE, uomId, uomIdTo, dispatcher);
    }
    
    public BigDecimal convertUom(BigDecimal originalValue, String uomId, String uomIdTo) {
        if (originalValue == null) return null;
        if (uomId != null && uomId.equals(uomIdTo)) return originalValue;
        BigDecimal rate = getConvertRate(uomId, uomIdTo);
        if (rate == null) return null;
        return originalValue.multiply(rate);
    }

}
