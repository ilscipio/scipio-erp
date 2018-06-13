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

final String module = "CmsContentTree.groovy";

/* Create Content Tree */
// Fetch controller entries
// NOTE: 2016: this is now filtered by cmsWebSiteIdSet meaning sites registered with process filter or screen view handler

// DEV NOTE: MOST OF OUR CODE CURRENTLY ASSUMES primaryPathFromContextRoot(Default)=Y
// This means by default we display /control prefix everywhere so ALL paths
// are relative to webapp context root, consistently.
// This is the simplest/best default because it allows the greatest flexibility with
// the simplest syntax (only a single path needed to express, no matter if page or request,
// always from webapp context root).

// FIXME?: running addWebSiteSettingsToMap twice before/after for now... oh well...
webSiteExtraConfigs = [:];
for(webSiteId in context.cmsWebSiteIdSet) {
    settingsMap = [:];
    context.cmsContentTreeUtil.addWebSiteSettingsToMap(settingsMap, webSiteId);
    webSiteExtraConfigs[webSiteId] = settingsMap;
}