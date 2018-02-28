/**
 * Gets SolrStatus entity (data status), the Solr webapp status info and some config info.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.solr.SolrUtil;

final module = "GetSolrStatusDetails.groovy";

pingWebapp = context.pingWebapp;
if (pingWebapp == null) pingWebapp = true;

solrWebappStatus = new LinkedHashMap();

setWebappStatus = { name, label, cb ->
    def info = [:];
    info.label = label;
    try {
        info.status = cb();
    } catch(Exception e) {
        Debug.logError(e, "Solr: Error determining Solr webapp '" + name + "' status: " + e.getMessage(), module);
        info.errMsg = e.getMessage() + " (" + e.getClass().getName() + ")";
    }
    solrWebappStatus[name] = info;
};

setWebappStatus("present", "SolrPresent", { SolrUtil.isSolrWebappPresent(); });
setWebappStatus("enabled", "SolrEnabled", { SolrUtil.isSolrWebappEnabled(); });
setWebappStatus("initialized", "SolrInitialized", { SolrUtil.isSolrWebappInitialized(); });
if (pingWebapp) {
    setWebappStatus("ready", "SolrReady", { SolrUtil.isSolrWebappReady(); });
}

context.solrWebappStatus = solrWebappStatus;

solrDataStatus = SolrUtil.getSolrStatus(delegator);
context.solrDataStatus = solrDataStatus;

solrConfigs = new LinkedHashMap();
addConfigEntry = { entry -> solrConfigs[entry.name] = entry; }
addConfigProp = { propName, title=null, label=null ->
    def value = UtilProperties.getPropertyValue(SolrUtil.getSolrConfigName(), propName, null);
    addConfigEntry([propName:propName, name:propName, title:title?:propName, value:value, label:label]);
};

addConfigProp("solr.version", "Solr Version");
addConfigEntry([name:"solr.config.version.effective", title:"Solr Config Version (Effective)", value:SolrUtil.getSolrConfigVersionStatic()]);
addConfigProp("solr.core.default", "Default core");
addConfigEntry([name:"Solr instance URL", value:SolrUtil.makeSolrWebappUrl()]);
addConfigEntry([name:"Default core URL", value:SolrUtil.makeSolrDefaultCoreUrl()]);
addConfigProp("solr.eca.enabled", "Solr ECAs enabled");

context.solrConfigs = solrConfigs;

solrConfigVersion = solrConfigs["solr.config.version.effective"]?.value;
context.solrConfigVersion = solrConfigVersion;






