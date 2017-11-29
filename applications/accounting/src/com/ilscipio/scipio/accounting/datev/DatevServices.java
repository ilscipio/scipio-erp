package com.ilscipio.scipio.accounting.datev;

import java.util.Map;

import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

public class DatevServices {

    public static String module = DatevServices.class.getName();

    /**
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> importDatevTransactionEntries(DispatchContext dctx, Map<String, Object> context) {
        // TODO: Implement import datev data from a csv
        return ServiceUtil.returnSuccess();
    }
    
    /**
     * 
     * @param dctx
     * @param context
     * @return
     */
    public static Map<String, Object> exportBookingStakcDatevData(DispatchContext dctx, Map<String, Object> context) {
        // TODO: Implement export datev data in csv format
        return ServiceUtil.returnSuccess();
    }

}
