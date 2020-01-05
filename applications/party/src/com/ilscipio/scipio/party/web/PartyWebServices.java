package com.ilscipio.scipio.party.web;

import com.ilscipio.scipio.web.SocketSessionManager;
import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import java.sql.Timestamp;
import java.util.*;

public class PartyWebServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static Map<String, Object> sendHitBinLiveData(DispatchContext dctx, Map<String, ? extends Object> context) {
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Delegator delegator = dctx.getDelegator();
        Map<String, Object> result = ServiceUtil.returnSuccess();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String channel = (String) context.get("channel");

        try {

            String interval = ((String) context.get("interval")).toLowerCase();
            Timestamp nowTimestamp = UtilDateTime.nowTimestamp();
            Timestamp begin = nowTimestamp;

            TimeZone timeZone = TimeZone.getDefault();
            Locale locale = Locale.getDefault();

            switch (interval){
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

            Map findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",begin,"thruDate",nowTimestamp,"dateInterval",interval,"userLogin",userLogin));

            JSON obj = JSON.from(findDataMap.get("requests"));
            SocketSessionManager.broadcastToChannel(obj.toString(),channel);

        }catch(Exception e){
            Debug.logError("Error while sending order data to websocket",module);
            return ServiceUtil.returnError("Error while sending order data to websocket");
        }

        return result;
    }

}
