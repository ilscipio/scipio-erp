import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.HashMap;

import javax.servlet.*;
import javax.servlet.http.*;

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.control.*;
import org.ofbiz.base.component.ComponentConfig;

import com.ilscipio.scipio.treeMenu.jsTree.JsTreeCore;
import com.ilscipio.scipio.treeMenu.jsTree.JsTreeHelper;
import com.ilscipio.scipio.cms.webapp.ComponentUtil;
import com.ilscipio.scipio.cms.control.CmsWebSiteInfo;
import com.ilscipio.scipio.cms.control.CmsViewMapping;

final String module = "CmsGetPageViewMappings.groovy";

/*Create view mappings tree*/
// Fetch controller entries
viewMaps = ComponentUtil.getWebappViewMaps(context.cmsWebSiteIdSet);
context.viewMaps = viewMaps;

for(viewMap in viewMaps.values()) {
    context.cmsContentTreeUtil.addWebSiteSettingsToMap(viewMap, viewMap.webSiteId);
}

// Fetch mappings from db
viewMappings = null;
try {
    viewMappings = CmsViewMapping.getWorker().findAll(delegator, false);
} catch (Exception e) {
    Debug.logError(e, module);
    context.cmsErrorHandler.addContextReadError(context, "Exception reading view mappings: " + e.getMessage());
}

if (viewMappings != null) {
    // top-level nodes
    for(currMap in viewMaps.values()) {
        state = currMap.state ?: [:];
        // checkbox disabled
        state.disabled = ("false".equals(currMap.enabled));
        state.checkbox_disabled = true;
        currMap.state = state;
    }

    // child nodes
    viewMappings.each{ viewMapping ->
        webSiteId = viewMapping.webSiteId;
        pageId = viewMapping.pageId;
        viewName = viewMapping.targetViewName;
        viewMappingId = viewMapping.getId();
        // TODO?: right now we ignore all those marked active=false because UI won't
        // allow toggling, not really that useful. will leave that for later.
        active = viewMapping.isActiveLogical();
        if (webSiteId && pageId && viewName && active) {
            id = webSiteId + "_" + viewName;
            if (viewMaps.containsKey(webSiteId)) {
                Map currMap = viewMaps.get(webSiteId);
                List currViews = currMap.views;
                if (currViews != null) {
                    viewEntry = ComponentUtil.getMapOfValue(currViews, "id", id);
                    if (viewEntry != null) {
                        // just put the pageId next to view name for now, prettify later
                        viewEntry.text = viewName + " -> " + pageId;

                        data = viewEntry.data ?: [:];

                        if (data.pageId) {
                            // 2016-12-07: WARN: MULTIPLE VIEW MAPPINGS FOR SAME VIEW not supported currently
                            // it can be fixed by removing + readding because the service tries to prevent duplicates
                            // when it adds new ones
                            Debug.logWarning("Cms: Discovered CmsViewMapping mapping (viewMappingId: " + viewMappingId +
                                ") having the same webSiteId (" + webSiteId + ") and targetViewName (" + viewName + ") " +
                                "as another view mapping; this is not supported and result may be unpredictable; try using " +
                                "the page view mapping select tree to first remove the mapping and then re-add it (note: the re-add is essential)", module);
                        }

                        data.pageId = pageId;
                        data.active = active;
                        data.viewMappingId = viewMappingId; // NOTE: not really used; for debugging
                        viewEntry.data = data;

                        // doesn't work unless tie_selection is false but that's sketchy
                        //state = viewEntry.state ?: [:];
                        //state.checked = true;
                        //viewEntry.state = state;

                        // doesn't work, seems to prevent uncheck and too low-level
                        //a_attr = viewEntry.a_attr ?: [:];
                        //a_attr["class"] = "jstree-checked";
                        //viewEntry.a_attr = a_attr;
                    }
                }
            }
        }
    }
}

