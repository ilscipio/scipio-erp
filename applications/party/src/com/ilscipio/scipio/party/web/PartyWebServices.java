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

    private static String getServerHostName(Map<String, ? extends Object> context) {
        String serverHostName = (String) context.get("serverHostName");
        if (UtilValidate.isEmpty(serverHostName)) {
            serverHostName = GeneralConfig.getLocalhostAddress().getHostName();
        }
        return serverHostName;
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
        String serverHostName = getServerHostName(context);

        List<String> processOnHosts = Arrays.asList(EntityUtilProperties.getPropertyValue("webtools", "sendHitBinLiveData.processOnHosts", "", delegator).split(","));
        if (!processOnHosts.isEmpty() && !processOnHosts.get(0).isEmpty() && !processOnHosts.contains(serverHostName)) {
            return ServiceUtil.returnSuccess();
        }

        DateFormat requestsDateFormat = getRequestsDateFormat();
        Boolean sendEmpty = (Boolean) context.get("sendEmpty");

        Timestamp nowTimestamp = UtilDateTime.nowTimestamp();
        int expireInterval = EntityUtilProperties.getPropertyAsInteger("webtools", "removeHitBinLiveData.expireInterval", 120, delegator);

        // If processAllServers is specified, everything defaults to the parameters; if not specified then things check webtools.properties
        Boolean processAllServers = (Boolean) context.get("processAllServers");
        Boolean processAllServersDefault = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.processAllServers", false, delegator);
        Boolean expireAllServers = (Boolean) context.get("expireAllServers");
        if (expireAllServers == null) {
            if (processAllServers != null) {
                expireAllServers = processAllServers;
            } else {
                expireAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "removeHitBinLiveData.expireAllServers", null, delegator);
                if (expireAllServers == null) {
                    expireAllServers = processAllServersDefault;
                }
            }
        }
        Boolean reportAllServers = (Boolean) context.get("reportAllServers");
        if (reportAllServers == null) {
            if (processAllServers != null) {
                reportAllServers = processAllServers;
            } else {
                reportAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.reportAllServers", null, delegator);
                if (reportAllServers == null) {
                    reportAllServers = processAllServersDefault;
                }
            }
        }
        if (processAllServers == null) {
            processAllServers = processAllServersDefault;
        }
        String execMode = (String) context.get("execMode");
        if (execMode == null) {
            execMode = EntityUtilProperties.getPropertyValue("webtools", "sendHitBinLiveData.execMode", "full", delegator);
        }
        if (!"full".equals(execMode)) {
            // TODO: NOT IMPLEMENTED: we don't have timing information to implement "report" without updates
            return ServiceUtil.returnSuccess();
        }

        // clean old records
        if ((nowTimestamp.getTime() - lastExpireTime.getTime()) > (expireInterval * 60 * 1000)) {
            try {
                Map<String, Object> invokeCtx = dctx.getModelService("removeHitBinLiveData").makeValid(context, ModelService.IN_PARAM);
                invokeCtx.put("nowTimestamp", nowTimestamp);
                invokeCtx.put("expireAllServers", expireAllServers);
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

            Timestamp fromDate;
            Timestamp thruDate = nowTimestamp;

            GenericValue recentStats = delegator.select("thruDate").from("ServerHitBucketStats").where("serverHostName", serverHostName).orderBy("-thruDate").maxRows(1).queryFirst();
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
            if (processAllServers) {
                invokeCtx.remove("serverHostName");
            } else {
                invokeCtx.put("serverHostName", serverHostName);
            }
            if (DEBUG || Debug.verboseOn()) {
                Debug.logInfo("sendHitBinLiveData: Running getServerRequests with: " + invokeCtx, module);
            }
            Map<String, Object> findDataMap = dispatcher.runSync("getServerRequests", invokeCtx);
            if (!ServiceUtil.isSuccess(findDataMap)) {
                return ServiceUtil.returnFailure("getServerRequests failed: " + ServiceUtil.getErrorMessage(findDataMap));
            }
            Map<String, Map<String, Object>> combinedRequests = UtilGenerics.castNonNull(findDataMap.get("requests"), Collections.emptyMap());
            Map<String, Map<String, Map<String, Object>>> serverRequests = UtilGenerics.cast(findDataMap.get("serverRequests"));
            if (DEBUG || Debug.verboseOn()) {
                Debug.logInfo("sendHitBinLiveData: Got " + combinedRequests.size() + " request date entries from getServerRequests", module);
            }
            if (UtilValidate.isEmpty(combinedRequests)) {
                if (sendEmpty == null) {
                    sendEmpty = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.sendEmpty", false, delegator);
                }
                if (!sendEmpty) {
                    return ServiceUtil.returnSuccess("No new server hits, aborting");
                }
            }

            // NOTE: This version of the entity causes more storage, but if bucketMinutes is reasonable shouldn't be problem
            if (serverRequests != null) {
                for(Map.Entry<String, Map<String, Map<String, Object>>> serverEntry : serverRequests.entrySet()) {
                    for (Map.Entry<String, Map<String, Object>> entry : serverEntry.getValue().entrySet()) {
                        Map<String, Object> values = entry.getValue();
                        Timestamp date = UtilDateTime.toTimestamp(entry.getKey(), requestsDateFormat);
                        if (DEBUG || Debug.verboseOn()) {
                            Debug.logInfo("sendHitBinLiveData: Creating ServerHitBucketStats with date: " + date + " from key '" + entry.getKey() + "'", module);
                        }
                        List<String> contentIds = UtilGenerics.cast(values.get("contentIds"));
                        GenericValue stats = delegator.makeValue("ServerHitBucketStats");
                        stats.put("serverHostName", serverEntry.getKey());
                        stats.put("date", date); // already adjusted to minutes
                        stats.put("bucketMinutes", bucketMinutes.longValue());
                        stats.put("count", values.get("count"));
                        stats.put("contentIds", (contentIds != null) ? String.join(",", contentIds) : null);
                        stats.put("fromDate", fromDate);
                        stats.put("thruDate", thruDate);
                        delegator.createSetNextSeqId(stats);
                    }
                }
            }

            /* TODO: read-only mode
            boolean reportAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.reportAllServers", false, delegator);
            if (reportAllServers) {
                // get the data for all the other servers, pre-generated (if available) - but possible some servers are late...
                List<GenericValue> statsList = delegator.from("ServerHitBucketStats")
                        .where(EntityCondition.makeCondition("serverHostName", EntityOperator.NOT_EQUAL, serverHostName),
                                EntityCondition.makeCondition("date", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate),
                                EntityCondition.makeCondition("date", EntityOperator.LESS_THAN, thruDate))
                        .orderBy("-date").cache(false).queryList(); // do not entity cache here, always may have reporting delays without
                if (statsList != null) {
                    for(GenericValue stats : statsList) {
                        collectSavedRequests(requests, stats, fromDate, thruDate, true, requestsDateFormat);
                    }
                }
            }
             */

            // send data
            JSON jsonData = JSON.from(combinedRequests);
            String jsonDataString = jsonData.toString();
            SocketSessionManager.getDefault().broadcastToChannel(jsonDataString, channel);

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
        String serverHostName = getServerHostName(context);
        Boolean expireAllServers = (Boolean) context.get("expireAllServers");
        if (expireAllServers == null) {
            expireAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "removeHitBinLiveData.expireAllServers", null, delegator);
            if (expireAllServers == null) {
                expireAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.processAllServers", false, delegator);
            }
        }

        int expiredCount = 0;
        Timestamp filterDate = UtilDateTime.addMinutesToTimestamp(nowTimestamp, -expireMinutes);
        //EntityCondition cond = EntityCondition.makeCondition("thruDate", EntityOperator.LESS_THAN, filterDate); // for json storage
        EntityCondition cond = EntityCondition.makeCondition("date", EntityOperator.LESS_THAN, filterDate);
        if (!expireAllServers) {
            cond = EntityCondition.makeCondition(EntityCondition.makeCondition("serverHostName", serverHostName), EntityOperator.AND, cond);
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
        String msg = "Expired " + expiredCount + " old ServerHitBucketStats records for " + (expireAllServers ? "all servers" : "server '" + serverHostName  + "'")
                + " older than " + filterDate;
        Debug.logInfo("removeHitBinLiveData: " + msg, module);
        return ServiceUtil.returnSuccess(msg);
    }

    public static Map<String, Object> removeHitBinLiveDataAll(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        String serverHostName = getServerHostName(context);
        Boolean expireAllServers = (Boolean) context.get("expireAllServers");
        if (expireAllServers == null) {
            expireAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "removeHitBinLiveData.expireAllServers", null, delegator);
            if (expireAllServers == null) {
                expireAllServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.processAllServers", false, delegator);
            }
        }
        int expiredCount;
        try {
            if (expireAllServers) {
                expiredCount = delegator.removeAll("ServerHitBucketStats");
            } else {
                expiredCount = delegator.removeByCondition("ServerHitBucketStats", EntityCondition.makeCondition("serverHostName", serverHostName));
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        String msg = "Expired " + expiredCount + " old ServerHitBucketStats records for " + (expireAllServers ? "all servers" : "server '" + serverHostName  + "'");
        Debug.logInfo("removeHitBinLiveDataAll: " + msg, module);
        return ServiceUtil.returnSuccess(msg);
    }

    public static Map<String, Object> getSavedHitBinLiveData(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        boolean useCache = Boolean.TRUE.equals(context.get("useCache"));
        Timestamp fromDate = (Timestamp) context.get("fromDate");
        Timestamp thruDate = (Timestamp) context.get("thruDate"); // may be null
        String serverHostName = getServerHostName(context);
        Boolean allServers = (Boolean) context.get("allServers");
        if (allServers == null) {
            allServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.reportAllServers", null, delegator);
            if (allServers == null) {
                allServers = EntityUtilProperties.getPropertyAsBoolean("webtools", "sendHitBinLiveData.processAllServers", false, delegator);
            }
        }
        boolean splitBuckets = !Boolean.FALSE.equals(context.get("splitBuckets")); // NOTE: currently always true (pre-refactor)
        Integer maxRequests = (Integer) context.get("maxRequests");
        try {
            EntityCondition cond = EntityCondition.makeCondition("date", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate);
            if (thruDate != null) {
                cond = EntityCondition.makeCondition(EntityCondition.makeCondition("date", EntityOperator.LESS_THAN, thruDate));
            }
            DateFormat requestsDateFormat = getRequestsDateFormat();
            if (allServers) {
                Map<String, Map<String, Map<String, Object>>> serverRequests = new HashMap<>();
                List<GenericValue> statsList = delegator.from("ServerHitBucketStats").where(cond).orderBy("-date").cache(useCache).maxRows(maxRequests).queryList();
                Map<String, Map<String, Object>> allRequests = new TreeMap<>();
                if (statsList != null) {
                    for(GenericValue stats : statsList) {
                        String statsServerName = stats.getString("serverHostName");
                        Map<String, Map<String, Object>> requests = serverRequests.get(statsServerName);
                        if (requests == null) {
                            requests = new TreeMap<>();
                            serverRequests.put(statsServerName, requests);
                        }
                        collectSavedRequests(requests, stats, fromDate, thruDate, splitBuckets, requestsDateFormat);
                        collectSavedRequests(allRequests, stats, fromDate, thruDate, splitBuckets, requestsDateFormat);
                    }
                }
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("serverRequests", serverRequests);
                result.put("requests", allRequests);
                return result;
            } else {
                Map<String, Map<String, Object>> requests = new TreeMap<>();
                List<GenericValue> statsList = delegator.from("ServerHitBucketStats")
                        .where(EntityCondition.makeCondition(EntityCondition.makeCondition("serverHostName", serverHostName), EntityOperator.AND, cond))
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
        String serverHostName = stats.getString("serverHostName");
        String requestsDateString = requestsDateFormat.format(stats.getTimestamp("date"));
        String contentIdsString = stats.getString("contentIds");

        Map<String, Object> statsInfo = requests.get(requestsDateString);
        if (statsInfo == null) {
            statsInfo = new HashMap<>();
            long count = stats.getLong("count");
            statsInfo.put("count", count);
            statsInfo.put("contentIds", (contentIdsString != null) ? new ArrayList<>(Arrays.asList(contentIdsString.split(","))) : null);
            requests.put(requestsDateString, statsInfo);
        } else {
            long count = ((Long) statsInfo.get("count"));
            long newCount = stats.getLong("count");
            statsInfo.put("count", count + newCount);
            List<String> contentIds = UtilGenerics.cast(statsInfo.get("contentIds"));
            if (contentIds == null) {
                contentIds = (contentIdsString != null) ? new ArrayList<>(Arrays.asList(contentIdsString.split(","))) : null;
                statsInfo.put("contentIds", contentIds);
            } else {
                List<String> newContentIds = (contentIdsString != null) ? Arrays.asList(contentIdsString.split(",")) : null;
                if (newContentIds != null && newContentIds.size() > 0) {
                    Set<String> mergedContentIds = new LinkedHashSet<>(contentIds);
                    mergedContentIds.addAll(newContentIds);
                    if (mergedContentIds.size() > contentIds.size()) { // keep same if unchanged
                        contentIds = new ArrayList<>(mergedContentIds);
                        statsInfo.put("contentIds", contentIds);
                    }
                }
            }
            if (DEBUG) {
                Debug.logInfo("collectSavedRequests: merged for date '" + requestsDateString + "', count was: " + count + ", added: " + newCount, module);
            }
        }
        requests.put(requestsDateString, statsInfo);
    }

    private static DateFormat getRequestsDateFormat() {
        return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm"); // not thread-safe
    }

}
