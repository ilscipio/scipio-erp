/**
 * SCIPIO: setup wizard static step properties (labels, icons)
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupWizardStaticProperties.groovy";

// if needed
//def setupStepList = SetupWorker.getStepsStatic(); // (excludes "finished")

// TODO: REVIEW: some of these could go in global styles

context.setupStepTitlePropMap = [
    "organization": "SetupOrganization",
    "store": "CommonStore",
    "user": "PartyParty",
    "accounting": "AccountingAccounting",
    "facility": "ProductFacility",
    "catalog": "ProductCatalog",
    "website": "SetupWebSite"
];

context.setupStepIconMap = [
    "organization": "fa-vcard", 
    "store": "fa-shopping-cart", 
    "user": "fa-users", 
    "accounting": "fa-balance-scale", 
    "facility": "fa-cube", 
    "catalog": "fa-sitemap", 
    "website": "fa-file-text",
    "default": "fa-info"
];

