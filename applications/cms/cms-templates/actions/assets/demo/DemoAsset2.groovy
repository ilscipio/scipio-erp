/**
 * SCIPIO: Demo asset dedicated script.
 */

import org.ofbiz.base.util.*;
import com.ilscipio.scipio.cms.util.CmsScriptUtil;

final module = "DemoAsset.groovy";

Debug.logInfo("NOTICE from " + module + ": we are running", module);

Debug.logInfo("Running SYSINFO script (by id)...", module);
context.logSystemInfo = true;
CmsScriptUtil.runCmsScriptById("SYSINFO", context);

Debug.logInfo("Running SYSINFO script (by name)...", module);
context.logSystemInfo = true;
CmsScriptUtil.runCmsScriptByName("SystemInfo", context);
