import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupOrganization.groovy";

SetupWorker setupWorker = context.setupWorker;
setupStep = context.setupStep;
partyId = context.orgPartyId;

organizationData = context.organizationData ?: [:];

//context.groupName = context.partyGroup?.groupName;

context.mailShipAddressContactMech = organizationData.mailShipAddressContactMech;
context.mailShipAddressContactMechPurposes = organizationData.mailShipAddressContactMechPurposes;
context.mailShipAddressStandaloneCompleted = organizationData.mailShipAddressStandaloneCompleted;

context.locationAddressesCompleted = organizationData.locationAddressesCompleted;

context.workPhoneContactMech = organizationData.workPhoneContactMech;
context.faxPhoneContactMech = organizationData.faxPhoneContactMech;
context.primaryEmailContactMech = organizationData.primaryEmailContactMech;
context.simpleContactMechsCompleted = organizationData.simpleContactMechsCompleted;

context.contactMechsCompleted = organizationData.contactMechsCompleted;

// true if explicit orgPartyId OR explicit newOrganization=Y flag OR failed create
organizationSelected = partyId || setupWorker?.isEffectiveNewRecordRequest(setupStep);
context.organizationSelected = organizationSelected;

