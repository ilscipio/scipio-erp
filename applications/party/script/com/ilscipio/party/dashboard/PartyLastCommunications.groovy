import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilValidate
import org.ofbiz.entity.util.EntityUtilProperties
import org.ofbiz.party.party.PartyHelper

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
communicationEventList = from("CommunicationEvent").orderBy("entryDate DESC").queryList();

for (communication in communicationEventList) {
    fromPersonFullName = "";
    if (UtilValidate.isNotEmpty(communication.partyIdFrom))
        fromPersonFullName = PartyHelper.getPartyName(communication.getRelatedOne("FromParty", false));
    toPersonFullName = "";
    if (UtilValidate.isNotEmpty(communication.partyIdTo))
        toPersonFullName = PartyHelper.getPartyName(communication.getRelatedOne("ToParty", false));
    commEventType = communication.getRelatedOne("CommunicationEventType", true);
    if (communication.subject==null) {
        subject = "n/a";
    } else {
        subject = (communication.subject.length() > 25) ? communication.subject.substring(0, 25) + "..." : communication.subject;
    }
    lastCommunications.add(UtilMisc.toMap("commEventId", communication.communicationEventId, "partyIdFrom", communication.partyIdFrom, "fromPersonFullName", fromPersonFullName, "partyIdTo", communication.partyIdTo, 
        "toPersonFullName", toPersonFullName, "subject", subject, "commEventType", commEventType.description, "date", communication.entryDate
    ));
}

// pagination for last communications list
viewIndex = Integer.valueOf(parameters.VIEW_INDEX  ?: 0);
viewSize = Integer.valueOf(parameters.VIEW_SIZE ?: EntityUtilProperties.getPropertyValue("widget", "widget.form.defaultViewSize", "10", delegator));
listSize = lastCommunications ? lastCommunications.size() : 0;

lowIndex = (viewIndex * viewSize) + 1;
highIndex = (viewIndex + 1) * viewSize;
highIndex = highIndex > listSize ? listSize : highIndex;
lowIndex = lowIndex > highIndex ? highIndex : lowIndex;

context.viewIndex = viewIndex;
context.viewSize = viewSize;
context.listSize = listSize;
context.lowIndex = lowIndex;
context.highIndex = highIndex;


context.lastCommunications = lastCommunications;
