import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.GenericValue
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject

public class TrackingCodeVisitData extends DataGeneratorGroovyBaseScript {
    private static final String module = "TrackingCodeVisitData.groovy";
    private final static String DEFAULT_WEBAPP_NAME = "shop";

    public String getDataType() {
        return null;
    }

    TrackingCodeVisitData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TRACKING VISIT DATA-=-=-=-", module);
    }

    public void init() {
        trackingCodeSourceList = delegator.findByAnd("Enumeration", ["enumTypeId" : "TRACKINGCODE_SRC"] , null, true);
        context.trackingCodeSourceList = trackingCodeSourceList;
    }

    public List prepareData(int index, AbstractDataObject data) throws Exception {
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