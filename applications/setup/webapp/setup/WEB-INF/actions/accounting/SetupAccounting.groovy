import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;

import com.ilscipio.scipio.setup.*;

final module = "SetupAccounting.groovy";


SetupWorker setupWorker = context.setupWorker;
setupStep = context.setupStep;
Debug.log("setupStep =======> " + setupStep);

accountingData = context.accountingData ?: [:];

topGlAccountId = accountingData.topGlAccountId;
context.topGlAccountId = topGlAccountId;

context.glAccountTypes = delegator.findByAnd("GlAccountType", [:], UtilMisc.toList("description"), true);
context.glAccountClasses = delegator.findByAnd("GlAccountClass", [:], UtilMisc.toList("description"), true);
context.glResourceTypes = delegator.findByAnd("GlResourceType", [:], UtilMisc.toList("description"), true);


// true if explicit userPartyId OR explicit newUser=Y flag OR failed create
glSelected = topGlAccountId || setupWorker?.isEffectiveNewRecordRequest(setupStep);
context.glSelected = glSelected;
Debug.log("glSelected =======> " + glSelected);