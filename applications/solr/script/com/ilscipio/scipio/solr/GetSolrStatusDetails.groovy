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
        info.status = cb(info);
    } catch(Exception e) {
        Debug.logError("Solr: Error determining Solr webapp '" + name + "' status: " + e.getMessage(), module);
        info.errMsg = e.getMessage() + " (" + e.getClass().getName() + ")";
        if (defStatus != null) info.status = defStatus;
    }
    solrStatus[name] = info;
};

setStatus("enabled", "SolrSolrEnabled", { SolrUtil.isSolrEnabled(); });
setStatus("systemInitialized", "SolrSystemInitialized", { SolrUtil.isSystemInitialized(); });
boolean solrWebappLocal = true;
setStatus("isLocal", "SolrIsWebappLocal", { solrWebappLocal = SolrUtil.isSolrWebappLocal(); return solrWebappLocal; }, null, SolrUtil.getSolrWebappUrl());
setStatus("localEnabled", "SolrLocalWebappEnabled", { info ->
    boolean res = SolrUtil.isSolrLocalWebappPresent();
    if (!solrWebappLocal && res) info.warnMsg = UtilProperties.getMessage("SolrUiLabels", "SolrRedundantLocalInstanceInfo", context.locale);
    return res;
});
setStatus("localInitialized", "SolrLocalWebappStarted", { info ->
    boolean res = SolrUtil.isSolrLocalWebappStarted();
    if (!solrWebappLocal && res) info.warnMsg = UtilProperties.getMessage("SolrUiLabels", "SolrRedundantLocalInstanceInfo", context.locale);
    return res;
});
if (pingWebapp) {
    setStatus("webappReady", "SolrWebappReady", { SolrUtil.isSolrWebappReadyRaw(); }, false, SolrUtil.getSolrWebappUrl());
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
addConfigEntry([name:"Solr ECAs enabled", value:SolrUtil.isSolrEcaEnabled(delegator), 
    dbValue:SolrUtil.getSolrEcaEnabledSystemProperty(delegator), specialType:"eca-toggle"]);

context.solrConfigs = solrConfigs;

solrConfigVersion = solrConfigs["solr.config.version.effective"]?.value;
context.solrConfigVersion = solrConfigVersion;






