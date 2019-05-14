/*
 Licensed to the Apache Software Foundation (ASF) under one
 or more contributor license agreements.  See the NOTICE file
 distributed with this work for additional information
 regarding copyright ownership.  The ASF licenses this file
 to you under the Apache License, Version 2.0 (the
 "License"); you may not use this file except in compliance
 with the License.  You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing,
 software distributed under the License is distributed on an
 "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 KIND, either express or implied.  See the License for the
 specific language governing permissions and limitations
 under the License.
 */

package org.ofbiz.common.uom;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;

import com.ibm.icu.util.Calendar;

/**
 * UomWorker
 */
public class UomWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private UomWorker () {}

    public static int[] uomTimeToCalTime(String uomId) {
        if ("TF_ms".equals(uomId)) {
            return new int[] { Calendar.MILLISECOND, 1 };
        } else if ("TF_s".equals(uomId)) {
            return new int[] { Calendar.SECOND, 1 };
        } else if ("TF_min".equals(uomId)) {
            return new int[] { Calendar.MINUTE, 1 };
        } else if ("TF_hr".equals(uomId)) {
            return new int[] { Calendar.HOUR, 1 };
        } else if ("TF_day".equals(uomId)) {
            return new int[] { Calendar.DAY_OF_YEAR, 1 };
        } else if ("TF_wk".equals(uomId)) {
            return new int[] { Calendar.WEEK_OF_YEAR, 1 };
        } else if ("TF_mon".equals(uomId)) {
            return new int[] { Calendar.MONTH, 1 };
        } else if ("TF_yr".equals(uomId)) {
            return new int[] { Calendar.YEAR, 1 };
        } else if ("TF_decade".equals(uomId)) {
            return new int[] { Calendar.YEAR, 10 };
        } else if ("TF_score".equals(uomId)) {
            return new int[] { Calendar.YEAR, 20 };
        } else if ("TF_century".equals(uomId)) {
            return new int[] { Calendar.YEAR, 100 };
        } else if ("TF_millenium".equals(uomId)) {
            return new int[] { Calendar.YEAR, 1000 };
        }

        return null;
    }

    public static Calendar addUomTime(Calendar cal, Timestamp startTime, String uomId, int value) {
        if (cal == null) {
            cal = Calendar.getInstance();
        }
        if (startTime != null) {
            cal.setTimeInMillis(startTime.getTime());
        }
        int[] conv = uomTimeToCalTime(uomId);

        // conversion multiplier * value by type
        cal.add(conv[0], (value * conv[1]));
        return cal;
    }

    public static Calendar addUomTime(Calendar cal, String uomId, int value) {
        return addUomTime(cal, null, uomId, value);
    }

    public static Calendar addUomTime(Timestamp startTime, String uomId, int value) {
        return addUomTime(null, startTime, uomId, value);
    }

    /**
    * SCIPIO: Method to use a conversion unit from a specific date
    */
   public static BigDecimal convertDatedUom(Timestamp timeStamp, BigDecimal originalValue, String uomId, String uomIdTo, LocalDispatcher dispatcher,boolean safe) {
       if (originalValue == null || uomId == null || uomIdTo == null) return null;
       if (uomId.equals(uomIdTo)) return originalValue;
       //BigDecimal conversionRate = BigDecimal.ONE;
       //BigDecimal convertedValue = BigDecimal.ZERO;
       //Delegator delegator = dispatcher.getDelegator();

       Map<String, Object> svcInMap = new LinkedHashMap<String, Object>();
       svcInMap.put("originalValue", originalValue);
       svcInMap.put("uomId", uomId);
       svcInMap.put("uomIdTo", uomIdTo);
       svcInMap.put("asOfDate", timeStamp);

       Map<String, Object> svcOutMap = new LinkedHashMap<String, Object>();
       try {
           // SCIPIO: support safe mode: create a new transaction to prevent screen crashes
           if (safe) {
               svcOutMap = dispatcher.runSync("convertUom", svcInMap, -1, true);
           } else {
               svcOutMap = dispatcher.runSync("convertUom", svcInMap);
           }
       } catch (GenericServiceException ex) {
           Debug.logError(ex, module);
           return null;
       }

       if (svcOutMap.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_SUCCESS) && svcOutMap.get("convertedValue") != null) {
           return (BigDecimal) svcOutMap.get("convertedValue");
       }
       Debug.logError("Failed to perform conversion for value [" + originalValue.toPlainString() + "] from Uom [" + uomId + "] to Uom [" + uomIdTo + "]",module);
       return null;
   }

    /**
     * Convenience method to call the convertUom service
     * <p>
     * SCIPIO: modified to support safe call
     */
    private static BigDecimal convertUom(BigDecimal originalValue, String uomId, String uomIdTo, LocalDispatcher dispatcher, boolean safe) {
        if (originalValue == null || uomId == null || uomIdTo == null) return null;
        if (uomId.equals(uomIdTo)) return originalValue;

        Map<String, Object> svcInMap =  new LinkedHashMap<>();
        svcInMap.put("originalValue", originalValue);
        svcInMap.put("uomId", uomId);
        svcInMap.put("uomIdTo", uomIdTo);

        Map<String, Object> svcOutMap;
        try {
            // SCIPIO: support safe mode: create a new transaction to prevent screen crashes
            if (safe) {
                svcOutMap = dispatcher.runSync("convertUom", svcInMap, -1, true);
            } else {
                svcOutMap = dispatcher.runSync("convertUom", svcInMap);
            }
        } catch (GenericServiceException ex) {
            Debug.logError(ex, module);
            return null;
        }

        if (svcOutMap.get(ModelService.RESPONSE_MESSAGE).equals(ModelService.RESPOND_SUCCESS) && svcOutMap.get("convertedValue") != null) {
            return (BigDecimal) svcOutMap.get("convertedValue");
        }
        Debug.logError("Failed to perform conversion for value [" + originalValue.toPlainString() + "] from Uom [" + uomId + "] to Uom [" + uomIdTo + "]",module);
        return null;
    }


    /**
     * Convenience method to call the convertUom service
     * <p>
     * SCIPIO: This is the original Ofbiz overload.
     */
    public static BigDecimal convertUom(BigDecimal originalValue, String uomId, String uomIdTo, LocalDispatcher dispatcher) {
        return convertUom(originalValue, uomId, uomIdTo, dispatcher, false);
    }

    /**
     * SCIPIO: Convenience method to call the convertUom service, which is always safe to call
     * from screens and will not throw exceptions or trigger transaction failures.
     */
    public static BigDecimal convertUomSafe(BigDecimal originalValue, String uomId, String uomIdTo, LocalDispatcher dispatcher) {
        return convertUom(originalValue, uomId, uomIdTo, dispatcher, true);
    }

    /**
     * SCIPIO: Returns Uom records of the requested type, but only those which have at least one
     * conversion record in the system. Used to prevent listing completely inconvertible Uom records
     * in the UI.
     * <p>
     * TODO: this is slow; should optimize somehow...
     *
     * @param asTarget ternary Boolean value: true means match only convertible-to Uoms, false means convertible-from, and null means both
     */
    public static List<GenericValue> getConvertibleUoms(Delegator delegator, LocalDispatcher dispatcher, Boolean asTarget,
            Map<String, ?> uomLookupFields, List<String> orderBy, boolean filterByDate, Timestamp dateFilterMoment, boolean cache) {
        try {
            List<GenericValue> uomList = delegator.findByAnd("Uom", uomLookupFields, orderBy, cache);
            List<GenericValue> resultUomList = new ArrayList<>(uomList.size());

            List<GenericValue> conversionList;
            for(GenericValue uom : uomList) {
                EntityCondition srcTargetCond;
                if (asTarget == Boolean.TRUE) {
                    srcTargetCond = EntityCondition.makeCondition("uomIdTo", uom.getString("uomId"));
                } else if (asTarget == Boolean.FALSE) {
                    srcTargetCond = EntityCondition.makeCondition("uomId", uom.getString("uomId"));
                } else {
                    srcTargetCond = EntityCondition.makeCondition(
                            EntityCondition.makeCondition("uomId", uom.getString("uomId")),
                            EntityOperator.OR,
                            EntityCondition.makeCondition("uomIdTo", uom.getString("uomId")));
                }

                conversionList = delegator.findList("UomConversion", srcTargetCond, null, null, null, cache);
                if (!conversionList.isEmpty()) {
                    resultUomList.add(uom);
                    continue;
                }

                EntityCondition cond = srcTargetCond;
                if (filterByDate) {
                    cond = EntityCondition.makeCondition(cond, EntityOperator.AND,
                            dateFilterMoment != null ? EntityUtil.getFilterByDateExpr(dateFilterMoment) : EntityUtil.getFilterByDateExpr());
                }
                conversionList = delegator.findList("UomConversionDated", cond, null, null, null, cache);
                if (!conversionList.isEmpty()) {
                    resultUomList.add(uom);
                    continue;
                }
            }

            return resultUomList;
        } catch(Exception e) {
            Debug.logError(e, module);
            return new ArrayList<>();
        }
    }

}
