package com.ilscipio.scipio.party.web;

import com.ibm.icu.util.Calendar;
import com.ilscipio.scipio.web.SocketSessionManager;
import org.ofbiz.base.GeneralConfig;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.*;

public class PartyWebServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final boolean DEBUG = UtilValidate.booleanValueVersatile(System.getProperty("scipio.websocket.debug"), false);

    /** TODO: REVIEW: lastExpireTime should really be stored in the DB, maybe could use JobSandbox, but seems like overkill, and every server needs one anyway. */
    private static volatile Timestamp lastExpireTime = UtilDateTime.nowTimestamp();

    private static String getServerName(Map<String, ? extends Object> context) {
        String serverName = (String) context.get("serverName");
        if (UtilValidate.isEmpty(serverName)) {
            serverName = GeneralConfig.getInstanceId();
        }
        return serverName;
    }

    public static Map<String, Object> sendHitBinLiveData(DispatchContext dctx, Map<String, ?> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        //TimeZone timeZone = TimeZone.getDefault();
        //Locale locale = Locale.getDefault();
        String channel = (String) context.get("channel");
        String interval = ((String) context.get("interval"));
        if (interval != null) {
            interval = interval.toLowerCase();
        }
        String serverName = getServerName(context);
        DateFormat requestsDateFormat = getRequestsDateFormat();
        Boolean sendEmpty = (Boolean) context.get("sendEmpty");

        Timestamp nowTimestamp = UtilDateTime.nowTimestamp();
        int expireInterval = EntityUtilProperties.getPropertyAsInteger("webtools", "removeHitBinLiveData.expireInterval", 120, delegator);

        // clean old records
        if ((nowTimestamp.getTime() - lastExpireTime.getTime()) > (expireInterval * 60 * 1000)) {
            try {
                Map<String, Object> invokeCtx = dctx.getModelService("removeHitBinLiveData").makeValid(context, ModelService.IN_PARAM);
                invokeCtx.put("nowTimestamp", nowTimestamp);
                Map<String, Object> findDataMap = dispatcher.runSync("removeHitBinLiveData", invokeCtx, -1, true);
                if (!ServiceUtil.isSuccess(findDataMap)) {
                    Debug.logError("sendHitBinLiveData: Failed to expire old records: " + ServiceUtil.getErrorMessage(findDataMap), module);
                }
            } catch(Exception e) {
                Debug.logError("sendHitBinLiveData: Failed to expire old records: " + e.toString(), module);
            }
        }

        try {
            Integer bucketMinutes = (Integer) context.get("bucketMinutes");
            if (bucketMinutes == null) {
                bucketMinutes = EntityUtilProperties.getPropertyAsInteger("webtools", "sendHitBinLiveData.bucketMinutes", 120, delegator);
            }
            long bucketMs = bucketMinutes * 60 * 1000;

            /* TODO: REVIEW: bucketMinutes is a lot easier for now, interval would need a different format and doesn't fit anymore
            Timestamp begin = nowTimestamp;
            switch (interval) {
                case "hour":
                    begin = UtilDateTime.getHourStart(nowTimestamp, 0, timeZone, locale);
                    break;

                case "day":
                    begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
                    break;

                case "week":
                    begin = UtilDateTime.getWeekStart(nowTimestamp, 0, timeZone, locale);
                    break;

                case "month":
                    begin = UtilDateTime.getMonthStart(nowTimestamp, 0, timeZone, locale);
                    break;

                case "year":
                    begin = UtilDateTime.getYearStart(nowTimestamp, 0, timeZone, locale);
                    break;

                default:
                    begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
            }
             */

            Timestamp fromDate;
            Timestamp thruDate = nowTimestamp;

            // find last data date
            /* previous json-based code
            GenericValue recentStats = delegator.select("thruDate").from("ServerHitBucketStats").where("serverName", serverName).orderBy("-thruDate").maxRows(1).queryFirst();
            if (recentStats != null) {
                fromDate = recentStats.getTimestamp("thruDate");
            } else {
                fromDate = UtilDateTime.adjustTimestamp(nowTimestamp, Calendar.MINUTE, -bucketMinutes);
            }*/
            GenericValue recentStats = delegator.select("thruDate").from("ServerHitBucketStats").where("serverName", serverName).orderBy("-thruDate").maxRows(1).queryFirst();
            if (recentStats != null) {
                fromDate = UtilDateTime.getMinuteBasedTimestamp(recentStats.getTimestamp("thruDate"));
                if (fromDate.getTime() != recentStats.getTimestamp("thruDate").getTime()) {
                    Debug.logWarning("sendHitBinLiveData: last ServerHitBucketStats.thruDate had non-zero seconds or milliseconds, automatically stripped: "
                            + recentStats.getTimestamp("thruDate"), module);
                }
                // don't run if bucket not elapsed
                if ((thruDate.getTime() - fromDate.getTime() < bucketMs)) {
                    if (DEBUG || Debug.verboseOn()) {
                        Debug.logVerbose("sendHitBinLiveData: Skipping stat generation - not enough time elapsed", module);
                    }
                    return ServiceUtil.returnSuccess("Skipping stat generation - not enough time elapsed");
                }
            } else {
                fromDate = UtilDateTime.getMinuteBasedTimestamp(UtilDateTime.adjustTimestamp(nowTimestamp, Calendar.MINUTE, -bucketMinutes));
            }
            // To keep date simple, set thruDate to match bucket (next bucket start), in other words "closest" bucket to nowTimestamp
            thruDate = UtilDateTime.getTimestamp(fromDate.getTime() + (((thruDate.getTime() - fromDate.getTime()) / bucketMs) * bucketMs)); // NOTE: integer division

            // get new data
            Map<String, Object> invokeCtx = dctx.getModelService("getServerRequests").makeValid(context, ModelService.IN_PARAM);
            invokeCtx.put("fromDate", fromDate);
            invokeCtx.put("thruDate", thruDate);
            invokeCtx.put("dateInterval", interval);
            invokeCtx.put("bucketMinutes", bucketMinutes);
            if (DEBUG || Debug.verboseOn()) {
                Debug.logInfo("sendHitBinLiveData: Running getServerRequests with: " + invokeCtx, module);
            }
            Map<String, Object> findDataMap = dispatcher.runSync("getServerRequests", invokeCtx);
            if (!ServiceUtil.isSuccess(findDataMap)) {
                return ServiceUtil.returnFailure("getServerRequests failed: " + ServiceUtil.getErrorMessage(findDataMap));
            }
            Map<String, Map<String, Object>> requests = UtilGenerics.cast(findDataMap.get("requests"));
            if (DEBUG || Debug.verboseOn()) {
                Debug.logInfo("sendHitBinLiveData: Got " + requests.size() + " request date entries from getServerRequests", module);
            }
            if (UtilValidate.isEmpty(requests)) {
                if (sendEmpty == null) {
                    sendEmpty = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.sendEmpty", false, delegator);
                }
                if (!sendEmpty) {
                    return ServiceUtil.returnSuccess("No new server hits, aborting");
                }
            }

            // TODO: REVIEW: I don't see great reason

            // save data
            /* old JSON storage code
            JSON jsonData = JSON.from(requests);
            String jsonDataString = jsonData.toString();
            GenericValue stats = delegator.makeValue("ServerHitBucketStats");
            stats.put("serverName", serverName);
            stats.put("fromDate", fromDate);
            stats.put("thruDate", thruDate);
            stats.put("bucketMinutes", bucketMinutes.longValue());
            stats.put("requests", jsonDataString);
            delegator.createSetNextSeqId(stats);
             */
            // NOTE: This version of the entity causes more storage, but if bucketMinutes is reasonable shouldn't be problem
            if (requests != null) {
                for(Map.Entry<String, Map<String, Object>> entry : requests.entrySet()) {
                    Map<String, Object> values = entry.getValue();
                    Timestamp date = UtilDateTime.toTimestamp(entry.getKey(), requestsDateFormat);
                    if (DEBUG || Debug.verboseOn()) {
                        Debug.logInfo("sendHitBinLiveData: Creating ServerHitBucketStats with date: " + date + " from key '" + entry.getKey() + "'", module);
                    }
                    List<String> contentIds = UtilGenerics.cast(values.get("contentIds"));
                    GenericValue stats = delegator.makeValue("ServerHitBucketStats");
                    stats.put("serverName", serverName);
                    stats.put("date", date); // already adjusted to minutes
                    stats.put("bucketMinutes", bucketMinutes.longValue());
                    stats.put("count", values.get("count"));
                    stats.put("contentIds", (contentIds != null) ? String.join(",", contentIds) : null);
                    stats.put("fromDate", fromDate);
                    stats.put("thruDate", thruDate);
                    delegator.createSetNextSeqId(stats);
                }
            }

            // send data
            JSON jsonData = JSON.from(requests);
            String jsonDataString = jsonData.toString();
            SocketSessionManager.broadcastToChannel(jsonDataString, channel);

        } catch (Exception e) {
            Debug.logError(e, "sendHitBinLiveData: Error while fetching or sending order data to websocket (note: if duplicate key constraint, you may need to delete old ServerHitBucketStats records)", module);
            return ServiceUtil.returnError("Error while fetching or sending order data to websocket (note: if duplicate key constraint, you may need to delete old ServerHitBucketStats records)");
        }

        return ServiceUtil.returnSuccess();
    }

    public static Map<String, Object> removeHitBinLiveData(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        Integer expireMinutes = (Integer) context.get("expireMinutes");
        if (expireMinutes == null) {
            expireMinutes = EntityUtilProperties.getPropertyAsInteger("webtools", "removeHitBinLiveData.expireMinutes", 1440, delegator);
        }
        Timestamp nowTimestamp = (Timestamp) context.get("nowTimestamp");
        if (nowTimestamp == null) {
            nowTimestamp = UtilDateTime.nowTimestamp();
        }
        String serverName = getServerName(context);
        Boolean expireAllServers = (Boolean) context.get("expireAllServers");
        if (expireAllServers == null) {
            expireAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "removeHitBinLiveData.expireAllServers", false, delegator);
        }

        int expiredCount = 0;
        Timestamp filterDate = UtilDateTime.addMinutesToTimestamp(nowTimestamp, -expireMinutes);
        //EntityCondition cond = EntityCondition.makeCondition("thruDate", EntityOperator.LESS_THAN, filterDate); // for json storage
        EntityCondition cond = EntityCondition.makeCondition("date", EntityOperator.LESS_THAN, filterDate);
        if (!expireAllServers) {
            cond = EntityCondition.makeCondition(EntityCondition.makeCondition("serverName", serverName), EntityOperator.AND, cond);
        }
        // NOTE: For now this doesn't use Delegator.removeByCondition (would be faster) because its clears whole entity from entity cache for no reason
        try(EntityListIterator it = delegator.from("ServerHitBucketStats").where(cond).queryIterator()) {
            GenericValue stats;
            while((stats = it.next()) != null) {
                stats.remove();
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        String msg = "Expired " + expiredCount + " old ServerHitBucketStats records for " + (expireAllServers ? "all servers" : "server '" + serverName  + "'")
                + " older than " + filterDate;
        Debug.logInfo("removeHitBinLiveData: " + msg, module);
        return ServiceUtil.returnSuccess(msg);
    }

    public static Map<String, Object> removeHitBinLiveDataAll(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        String serverName = getServerName(context);
        Boolean expireAllServers = (Boolean) context.get("expireAllServers");
        if (expireAllServers == null) {
            expireAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "removeHitBinLiveData.expireAllServers", false, delegator);
        }
        int expiredCount;
        try {
            if (expireAllServers) {
                expiredCount = delegator.removeAll("ServerHitBucketStats");
            } else {
                expiredCount = delegator.removeByCondition("ServerHitBucketStats", EntityCondition.makeCondition("serverName", serverName));
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        String msg = "Expired " + expiredCount + " old ServerHitBucketStats records for " + (expireAllServers ? "all servers" : "server '" + serverName  + "'");
        Debug.logInfo("removeHitBinLiveDataAll: " + msg, module);
        return ServiceUtil.returnSuccess(msg);
    }

    public static Map<String, Object> getSavedHitBinLiveData(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));
        Timestamp fromDate = (Timestamp) context.get("fromDate");
        Timestamp thruDate = (Timestamp) context.get("thruDate"); // may be null
        String serverName = getServerName(context);
        boolean allServers = Boolean.TRUE.equals(context.get("allServers"));
        boolean splitBuckets = !Boolean.FALSE.equals(context.get("splitBuckets"));
        Integer maxRequests = (Integer) context.get("maxRequests");
        try {
            /* previous JSON-based code, too complicated to manage the dates
            // Reconstitute the JSON requests
            EntityCondition cond = EntityCondition.makeCondition(EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate), EntityOperator.OR,
                    EntityCondition.makeCondition("thruDate", EntityOperator.GREATER_THAN, fromDate));
            if (thruDate != null) {
                cond = EntityCondition.makeCondition(cond, EntityOperator.AND,
                        EntityCondition.makeCondition(EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, thruDate)));
            }
            if (allServers) {
                Map<String, Map<String, Object>> serverRequests = new HashMap<>();
                List<GenericValue> statsList = delegator.from("ServerHitBucketStats").where(cond).cache(useCache).queryList();
                if (statsList != null) {
                    for(GenericValue stats : statsList) {
                        String statsServerName = stats.getString("serverName");
                        Map<String, Object> requests = serverRequests.get(statsServerName);
                        if (requests == null) {
                            requests = new LinkedHashMap<>();
                            serverRequests.put(statsServerName, requests);
                        }
                        collectSavedRequests(requests, stats, fromDate, thruDate, splitBuckets);
                    }
                }
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("serverRequests", serverRequests);
                result.put("requests", serverRequests);
                return result;
            } else {
                Map<String, Object> requests = new LinkedHashMap<>();
                List<GenericValue> statsList = delegator.from("ServerHitBucketStats")
                        .where(EntityCondition.makeCondition(EntityCondition.makeCondition("serverName", serverName), EntityOperator.AND, cond))
                        .cache(useCache).queryList();
                if (statsList != null) {
                    for(GenericValue stats : statsList) {
                        collectSavedRequests(requests, stats, fromDate, thruDate, splitBuckets);
                    }
                }
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("requests", requests);
                return result;
            }
             */
            EntityCondition cond = EntityCondition.makeCondition("date", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate);
            if (thruDate != null) {
                cond = EntityCondition.makeCondition(EntityCondition.makeCondition("date", EntityOperator.LESS_THAN, thruDate));
            }
            DateFormat requestsDateFormat = getRequestsDateFormat();
            if (allServers) {
                Map<String, Map<String, Map<String, Object>>> serverRequests = new HashMap<>();
                List<GenericValue> statsList = delegator.from("ServerHitBucketStats").where(cond).orderBy("-date").cache(useCache).maxRows(maxRequests).queryList();
                if (statsList != null) {
                    for(GenericValue stats : statsList) {
                        String statsServerName = stats.getString("serverName");
                        Map<String, Map<String, Object>> requests = serverRequests.get(statsServerName);
                        if (requests == null) {
                            requests = new TreeMap<>();
                            serverRequests.put(statsServerName, requests);
                        }
                        collectSavedRequests(requests, stats, fromDate, thruDate, splitBuckets, requestsDateFormat);
                    }
                }
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("serverRequests", serverRequests);
                result.put("requests", serverRequests);
                return result;
            } else {
                Map<String, Map<String, Object>> requests = new TreeMap<>();
                List<GenericValue> statsList = delegator.from("ServerHitBucketStats")
                        .where(EntityCondition.makeCondition(EntityCondition.makeCondition("serverName", serverName), EntityOperator.AND, cond))
                        .orderBy("-date").cache(useCache).maxRows(maxRequests).queryList();
                if (statsList != null) {
                    for(GenericValue stats : statsList) {
                        collectSavedRequests(requests, stats, fromDate, thruDate, splitBuckets, requestsDateFormat);
                    }
                }
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("requests", requests);
                return result;
            }
        } catch(GenericEntityException e) {
            Debug.logError(e, "getSavedHitBinLiveData: error getting ServerHitBucketStats requests", module);
            return ServiceUtil.returnFailure("Error getting ServerHitBucketStats requests");
        }
    }

    private static void collectSavedRequests(Map<String, Map<String, Object>> requests, GenericValue stats, Timestamp fromDate, Timestamp thruDate, boolean splitBuckets, DateFormat requestsDateFormat) {
        /* previous JSON storage code
        // NOTE: The ServerHitBucketStats entries for a server should always have exclusive dates, so there's no need to deep-merge entries, only filter the dates
        Map<String, Object> entityRequests = stats.getJsonAsMap("requests");
        if (splitBuckets) {
            for(Map.Entry<String, Object> entry : entityRequests.entrySet()) {
                Timestamp statsEntryTime;
                try {
                    statsEntryTime = toTimestamp(entry.getKey()); // NOTE: this was wrong!!
                } catch (Exception e) {
                    Debug.logError("getSavedHitBinLiveData: error parsing requests json date in ServerHitBucketStats '" + stats.get("statsId") + "': "
                            + e.getMessage(), module);
                    continue;
                }
                if (!statsEntryTime.before(fromDate) && (thruDate == null || statsEntryTime.before(thruDate))) {
                    requests.put(entry.getKey(), entry.getValue());
                }
            }
        } else {
            requests.putAll(entityRequests);
        }
         */
        String serverName = stats.getString("serverName");
        String requestsDateString = requestsDateFormat.format(stats.getTimestamp("date"));
        String contentIdsString = stats.getString("contentIds");

        Map<String, Object> statsInfo = new HashMap<>();
        statsInfo.put(requestsDateString, statsInfo);
        statsInfo.put("count", stats.getLong("count").intValue());
        statsInfo.put("contentIds", contentIdsString != null ? new ArrayList<>(Arrays.asList(contentIdsString.split(","))) : null);
        if (requests.containsKey(requestsDateString)) {
            Debug.logWarning("getSavedHitBinLiveData: Duplicate date in ServerHitBucketStats stats, ignoring previous: " + requestsDateString, module); // shouldn't really happen
        }
        requests.put(requestsDateString, statsInfo);
    }

    private static DateFormat getRequestsDateFormat() {
        return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm"); // not thread-safe
    }

}
