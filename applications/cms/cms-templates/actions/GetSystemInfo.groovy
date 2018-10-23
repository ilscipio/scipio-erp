/**
 * SCIPIO: Simple groovy script that gets some system info.
 */

import org.ofbiz.base.util.*;
 
final module = "GetSystemInfo.groovy";

scipioReleaseInfo = [:];

scipioReleaseInfo.name = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.name");
scipioReleaseInfo.group = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.group");
scipioReleaseInfo.desc = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.desc");
scipioReleaseInfo.branch = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.branch");
scipioReleaseInfo.version = UtilProperties.getPropertyValue("scipiometainfo.properties", "scipio.release.version");

if (context.logSystemInfo) {
    Debug.logInfo("Scipio version info: " + scipioReleaseInfo, module);
    context.remove("logSystemInfo");
}

context.scipioReleaseInfo = scipioReleaseInfo;