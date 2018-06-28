/**
 * Scipio CMS Get Media - script
 * Fetches Media files and puts them into context
 */

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;

import org.ofbiz.service.LocalDispatcher;

final String module = "CmsGetMediaList.groovy";

/*Parameters*/
int viewSize = (context.viewSize ?: 0) as Integer;
int viewIndex = (context.viewIndex ?: 0) as Integer;

simpleSearchText = (parameters.simpleSearchText ?: null) as String;
inputFields = null;
if (simpleSearchText) {
    inputFields = [
        "contentName": simpleSearchText,
        "contentName_op": "contains",
        "contentName_ic": "Y"
    ];
}

mediaResult = dispatcher.runSync("cmsGetMediaFiles", ["userLogin": context.userLogin, "locale": context.locale, 
    "viewSize":viewSize, "viewIndex":viewIndex, "inputFields":inputFields]);
if (ServiceUtil.isSuccess(mediaResult)) {
    mediaFiles = mediaResult.mediaFiles;
    
    context.viewSize = mediaResult.viewSize;
    context.viewIndex = mediaResult.viewIndex;
    context.listSize = mediaResult.listSize;
    context.mediaFiles = mediaFiles;
} else {
    context.cmsErrorHandler.addContextReadErrorFromServiceResult(context, mediaResult);
}
