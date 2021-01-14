/** SCIPIO */

resourceFiles = [:];
try {
    resourceFiles = org.ofbiz.webtools.labelmanager.LabelManagerFactory.findLabelFiles(false);
} catch(Exception e) {
    Debug.logError(e, "EntityLabels.groovy");
}
context.resourceFiles = resourceFiles;

resourceId = (parameters.resourceId ?: null) as String;

resourceInfo = null;
if (resourceId) {
    resourceInfo = resourceFiles[resourceId];
    if (resourceInfo != null) {
        resourceInfo = resourceInfo.toLocal();
    }
}
context.resourceInfo = resourceInfo;

if (resourceId && resourceInfo == null) {
    resourceId = null;
    errMsgList = context.errorMessageList ?: [];
    errMsgList.add("Could not find resource [" + resourceId + "]");
    context.errorMessageList = errMsgList;
}
context.resourceId = resourceId;
