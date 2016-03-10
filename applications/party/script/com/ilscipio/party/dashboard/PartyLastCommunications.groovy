import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime

String iScope = context.intervalScope != null ? context.intervalScope : "month"; //day|week|month|year

Calendar calendar = Calendar.getInstance();
if (iScope.equals("day")) {
    calendar.set(Calendar.DAY_OF_YEAR, calendar.get(Calendar.DAY_OF_YEAR) - 30);
} else if (iScope.equals("week")) {
    calendar.set(Calendar.DAY_OF_WEEK, 1);
    calendar.set(Calendar.WEEK_OF_YEAR, calendar.get(Calendar.WEEK_OF_YEAR) - 12);
} else if (iScope.equals("month")) {
    calendar.set(Calendar.DAY_OF_MONTH, 1);
    calendar.set(Calendar.MONTH, calendar.get(Calendar.MONTH) - 6);
} else if (iScope.equals("year")) {
    calendar.set(Calendar.DAY_OF_YEAR, 1);
    calendar.set(Calendar.MONTH, 1);
    calendar.set(Calendar.YEAR, calendar.get(Calendar.YEAR) - 5);
}
fromDate = UtilDateTime.toTimestamp(calendar.getTime());



lastCommunications = [];

// TODO: I believe we should improve this at some point by filtering status, fromDate, etc.
lastCommunications = from("CommunicationEvent").queryList();
Debug.log("lastCommunications ===========> " + lastCommunications);

context.lastCommunications = lastCommunications;