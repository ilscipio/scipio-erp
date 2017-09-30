import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;

organizationData = context.organizationData ?: [:];

//context.groupName = context.partyGroup?.groupName;

context.mailShipAddressContactMech = organizationData.mailShipAddressContactMech;
context.mailShipAddressContactMechPurposes = organizationData.mailShipAddressContactMechPurposes;
context.mailShipAddressCompleted = organizationData.mailShipAddressCompleted;

context.workPhoneContactMech = organizationData.workPhoneContactMech;
context.faxPhoneContactMech = organizationData.faxPhoneContactMech;
context.primaryEmailContactMech = organizationData.primaryEmailContactMech;
context.simpleContactMechsCompleted = organizationData.simpleContactMechsCompleted;

context.contactMechsCompleted = organizationData.contactMechsCompleted;
