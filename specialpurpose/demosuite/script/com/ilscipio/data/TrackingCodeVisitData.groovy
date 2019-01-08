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
    private static final String module = "TrackingCodeVisitData.groovy";
    private final static String DEFAULT_WEBAPP_NAME = "shop";

    TrackingCodeVisitData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TRACKING VISIT DATA-=-=-=-", module);
    }

    public void init() {
        trackingCodeSourceList = delegator.findByAnd("Enumeration", ["enumTypeId" : "TRACKINGCODE_SRC"] , null, true);
        context.trackingCodeSourceList = trackingCodeSourceList;
    }

    List prepareData(int index) throws Exception {
        List<GenericValue> toBeStored = new ArrayList<GenericValue>();

        if (context.trackingCodeSourceList) {
            trackingCodeSource =  context.trackingCodeSourceList.get(UtilRandom.random(context.trackingCodeSourceList));
            fromDate = UtilRandom.generateRandomTimestamp(context);

            String newSeqId = delegator.getNextSeqId("Visit");
            GenericValue visit = delegator.makeValue("Visit");
            visit.set("visitId", newSeqId);
            //        visit.set("sessionId", session.getId());
            visit.set("fromDate", fromDate);
            visit.set("initialLocale", context.locale.getDisplayName());
            visit.set("webappName", DEFAULT_WEBAPP_NAME);
            toBeStored.add(visit);

            trackingCodeList = delegator.findByAnd("TrackingCode", null, null, false);
            trackingCode = trackingCodeList.get(UtilRandom.random(trackingCodeList));

            GenericValue trackingCodeVisit = delegator.makeValue("TrackingCodeVisit",
                    UtilMisc.toMap("trackingCodeId", trackingCode.trackingCodeId, "visitId", visit.visitId,
                    "fromDate", fromDate, "sourceEnumId", trackingCodeSource.enumId));
            toBeStored.add(trackingCodeVisit);
        }
        return toBeStored;
    }
}