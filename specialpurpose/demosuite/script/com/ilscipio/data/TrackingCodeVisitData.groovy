import java.sql.Timestamp

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript


public class TrackingCodeVisitData extends DataGeneratorGroovyBaseScript {

    LinkedList prepareData() {
        final String DEFAULT_WEBAPP_NAME = "shop";
        
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TRACKING VISIT DATA-=-=-=-", "");

        trackingCodeSourceList = delegator.findByAnd("Enumeration", ["enumTypeId" : "TRACKINGCODE_SRC"] , null, true);
        if (trackingCodeSourceList)
            trackingCodeSource =  trackingCodeSourceList.get(UtilRandom.random(trackingCodeSourceList));
        
        Map result = ServiceUtil.returnSuccess();

        List<GenericValue> toBeStored = new LinkedList<GenericValue>();
        Locale locale = context.locale;

        minDate = context.minDate;
        //    maxDate = context.maxDate;
        if (!minDate) {
            calendar = UtilDateTime.toCalendar(UtilDateTime.nowTimestamp(), context.timeZone, context.locale);
            calendar.set(Calendar.MONTH, -6);
            minDate = UtilDateTime.getTimestamp(calendar.getTimeInMillis());
        }
        //    if (!thruDate) {
        //        thruDate = UtilDateTime.nowTimestamp();
        //    }

        for (int i = 0; i < numRecords; i++) {
            fromDate = UtilRandom.generateRandomDate(UtilDateTime.toDate(minDate), context);

            String newSeqId = delegator.getNextSeqId("Visit");
            GenericValue visit = delegator.makeValue("Visit");
            visit.set("visitId", newSeqId);
            //        visit.set("sessionId", session.getId());
            visit.set("fromDate", UtilDateTime.nowTimestamp());
            visit.set("initialLocale", locale.getDisplayName());
            visit.set("webappName", DEFAULT_WEBAPP_NAME);
            toBeStored.add(visit);

            trackingCodeList = delegator.findByAnd("TrackingCode", null, null, false);
            trackingCode = trackingCodeList.get(UtilRandom.random(trackingCodeList));
            if (trackingCodeSourceList)
                trackingCodeSource =  trackingCodeSourceList.get(UtilRandom.random(trackingCodeSourceList));

            GenericValue trackingCodeVisit = delegator.makeValue("TrackingCodeVisit",
                    UtilMisc.toMap("trackingCodeId", trackingCode.trackingCodeId, "visitId", visit.visitId,
                    "fromDate", Timestamp.valueOf(fromDate), "sourceEnumId", trackingCodeSource.enumId));
            toBeStored.add(trackingCodeVisit);
        }
        return toBeStored;
    }
}


