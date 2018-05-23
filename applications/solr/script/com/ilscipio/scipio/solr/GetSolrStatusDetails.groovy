/**
 * Gets SolrStatus entity (data status), the Solr webapp status info and some config info.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.solr.SolrUtil;

final module = "GetSolrStatusDetails.groovy";

pingWebapp = context.pingWebapp;
if (pingWebapp == null) pingWebapp = true;

solrStatus = new LinkedHashMap();

setStatus = { name, label, cb, defStatus=null, msg=null ->
    def info = [:];
    info.label = label;
    if (msg) info.msg = msg;
    try {
        info.status = cb();
    } catch(Exception e) {
        Debug.logError("Solr: Error determining Solr webapp '" + name + "' status: " + e.getMessage(), module);
        info.errMsg = e.getMessage() + " (" + e.getClass().getName() + ")";
        if (defStatus != null) info.status = defStatus;
    }
    solrStatus[name] = info;
};

setStatus("enabled", "SolrSolrEnabled", { SolrUtil.isSolrEnabled(); });
setStatus("systemInitialized", "SolrSystemInitialized", { SolrUtil.isSystemInitialized(); });
setStatus("isLocal", "SolrIsWebappLocal", { SolrUtil.isSolrWebappLocal(); }, null, SolrUtil.getSolrWebappUrl());
setStatus("localEnabled", "SolrLocalWebappEnabled", { SolrUtil.isSolrLocalWebappPresent(); });
setStatus("localInitialized", "SolrLocalWebappStarted", { SolrUtil.isSolrLocalWebappStarted(); });
if (pingWebapp) {
    setStatus("webappReady", "SolrWebappReady", { SolrUtil.isSolrWebappReadyRaw(); }, false);
}

context.solrStatus = solrStatus;

solrDataStatus = SolrUtil.getSolrStatus(delegator);
context.solrDataStatus = solrDataStatus;

solrConfigs = new LinkedHashMap();
addConfigEntry = { entry -> solrConfigs[entry.name] = entry; }
addConfigProp = { propName, title=null, label=null ->
    def value = UtilProperties.getPropertyValue(SolrUtil.getSolrConfigName(), propName, null);
    addConfigEntry([propName:propName, name:propName, title:title?:propName, value:value, label:label]);
};

addConfigProp("solr.enabled", "Solr Enabled");
addConfigProp("solr.version", "Solr Version");
addConfigEntry([name:"solr.config.version.effective", title:"Solr Config Version (Effective)", value:SolrUtil.getSolrConfigVersionStatic()]);
addConfigProp("solr.core.default", "Default core");
addConfigEntry([name:"Solr instance URL", value:SolrUtil.makeSolrWebappUrl()]);
addConfigEntry([name:"Default core URL", value:SolrUtil.makeSolrDefaultCoreUrl()]);
addConfigProp("solr.eca.enabled", "Solr ECAs enabled");

context.solrConfigs = solrConfigs;

solrConfigVersion = solrConfigs["solr.config.version.effective"]?.value;
context.solrConfigVersion = solrConfigVersion;






