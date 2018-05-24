import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.setup.*;


final module = "SetupAccounting.groovy";

SetupWorker setupWorker = context.setupWorker;
setupStep = context.setupStep;

/*
 * Request states
 */
eventFlags = setupWorker?.getRecordRequestStatesMap(["New", "Create", "Update", "Delete", "Copy", "Add"], true, ["GlAccount"]);
isEventError = context.isSetupEventError;

initialSettings = context.egltInitialSettings ?: [:];
if (initialSettings.noShowFormChange == null && initialSettings.noShowFormPopulate == null) { // caller can override if need
    initialSettings.noShowFormPopulate = false;
    initialSettings.noShowFormChange = false;
    if (isEventError) {
        initialSettings.noShowFormPopulate = true;
        initialSettings.noShowFormChange = true;
    }
}
context.elgtInitialSettings = initialSettings;

objectLocalizedFields = context.egltObjectLocalizedFields;
if (!objectLocalizedFields) {
    objectLocalizedFields = [
        glAccount: [
            fieldNames: ["accountName", "description"],
            typeNames: ["ACCOUNT_NAME", "DESCRIPTION"],
            typeNameListStr: '["ACCOUNT_NAME", "DESCRIPTION"]'
        ]
    ];
}
context.egltObjectLocalizedFields = objectLocalizedFields;


accountingData = context.accountingData ?: [:];

/*
 * Preferences
 */
acctgPreferences = accountingData.acctgPreferences;
context.acctgPreferences = acctgPreferences;


/*
 * GlAccount
 */
topGlAccountId = accountingData.topGlAccountId;
context.topGlAccountId = topGlAccountId;

topGlAccount = delegator.findOne("GlAccount", [glAccountId: topGlAccountId], false);
context.topGlAccount = topGlAccount;

/*
 * AcctgTransType & AcctgTransEntryType
 */
acctgTransTypes = EntityQuery.use(delegator).from("AcctgTransType").queryList();
context.acctgTransTypes = acctgTransTypes;
acctgTransEntryTypes = EntityQuery.use(delegator).from("AcctgTransEntryType").queryList();
context.acctgTransEntryTypes = acctgTransEntryTypes;

/*
 * Scipio store urls - Accounting addons
 */
Properties generalProps = UtilProperties.getProperties("general");
Map<String, String> scipioAcctgStandardAddons = [:];
for (String key in generalProps.stringPropertyNames()) {
    if (key.startsWith("scipio.store.addon.accounting")) {
        scipioAcctgStandardAddons.put(key.substring(key.lastIndexOf(".") + 1, key.length()), UtilProperties.getPropertyValue("general", "scipio.store.base.url") + generalProps.get(key));
    }
}
context.scipioAcctgStandardAddons = scipioAcctgStandardAddons; 
for (String key in scipioAcctgStandardAddons.keySet()) {
    Debug.log("[" + key + "]: " + scipioAcctgStandardAddons.get(key));
}


/*
 * Extra prep
 */

// TODO: REVIEW: this may not be covering all cases properly... some bad cases may be being hidden by jstree logic
// this is ignoring the delete/expire/copy/move actions because they are hidden forms
eventFlags.targetRecord = "glAccount";
if (eventFlags.isAddFailed) eventFlags.formActionType = "add";
else eventFlags.formActionType = (context.topGlAccount == null) ? "new" : "edit";
eventFlags.targetRecordAction = eventFlags.targetRecord + "-" + eventFlags.formActionType; // easier to check in ftl

// dump flags in context (FIXME?: remove this later)
context.putAll(eventFlags);

eventStates = [:];
eventStates.putAll(eventFlags);

// add some more (not in context)
eventStates.isError = context.isEventError;

context.eventStates = eventStates;

context.glAccountTypes = EntityQuery.use(delegator).from("GlAccountType").orderBy("description").queryList();
context.glAccountClasses = EntityQuery.use(delegator).from("GlAccountClass").orderBy("description").queryList();
context.glResourceTypes = EntityQuery.use(delegator).from("GlResourceType").orderBy("description").queryList();

context.periodTypes = EntityQuery.use(delegator).from("PeriodType").orderBy("description").queryList();
allCustomTimePeriods = from("CustomTimePeriod").orderBy(["organizationPartyId", "parentPeriodId", "periodTypeId", "periodNum", "fromDate"]).queryList();
context.allCustomTimePeriods = allCustomTimePeriods;


// true if explicit newGlAccount=Y flag OR failed create
//glSelected = topGlAccount || setupWorker?.isEffectiveNewRecordRequest(StringUtils.capitalize("GlAccount"));
//context.glSelected = glSelected;