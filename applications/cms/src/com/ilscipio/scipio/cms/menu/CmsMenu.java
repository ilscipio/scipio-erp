package com.ilscipio.scipio.cms.menu;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.lang.JSON;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.control.CmsControlUtil;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelations;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.data.CmsObjectCache;
import com.ilscipio.scipio.cms.data.CmsObjectCache.CacheEntry;

/**
 * CMS Menu Object
 */
public class CmsMenu extends CmsDataObject implements CmsMajorObject {

    private static final long serialVersionUID = -714021324721469544L;
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final CmsObjectCache<CmsMenu> idCache = CmsObjectCache.getGlobalCache("cms.menu.id");
    private static final CmsObjectCache<CmsMenu> nameCache = CmsObjectCache.getGlobalCache("cms.menu.name");
    
    protected List<Object> parsedMenuJsonList;

    protected CmsMenu(GenericValue entity) {
        super(entity);
    }

    protected CmsMenu(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }

    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
        getParsedMenuJsonList();
    }

    public String getWebsiteId() {
        return getEntityWebsiteId();
    }

    public String getEntityWebsiteId() {
        return this.entity.getString("websiteId");
    }

    void setEntityWebsiteId(String websiteId) {
        this.entity.setString("websiteId", websiteId);
    }

    public String getMenuName() {
        return getEntityMenuName();
    }

    public String getEntityMenuName() {
        return this.entity.getString("menuName");
    }

    void EntityMenuName(String menuName) {
        this.entity.setString("menuName", menuName);
    }

    public String getMenuDescr() {
        return getEntityMenuDescr();
    }

    public String getEntityMenuDescr() {
        return this.entity.getString("description");
    }

    void EntityMenuDescr(String description) {
        this.entity.setString("description", description);
    }

    public String getMenuJson() {
        return getEntityMenuJson();
    }

    public String getEntityMenuJson() {
        return this.entity.getString("menuJson");
    }

    void EntityMenuJson(String menuJson) {
        this.entity.setString("menuJson", menuJson);
    }

    @SuppressWarnings("unchecked")
    public List<Object> getParsedMenuJsonList() {
        List<Object> parsedMenuJsonList = this.parsedMenuJsonList;
        if (parsedMenuJsonList == null) {
            try {
                parsedMenuJsonList = (List<Object>) JSON.from(getEntityMenuJson())
                        .toObject(ArrayList.class);
            } catch(IOException e) {
                Debug.logError(e, "Could not parse JSON string: " + getEntityMenuJson(), module);
                parsedMenuJsonList = Collections.emptyList();
            }
            this.parsedMenuJsonList = parsedMenuJsonList;
        }
        return parsedMenuJsonList;
    }
    
    @Override
    public MenuWorker getWorkerInst() {
        return MenuWorker.worker;
    }

    public static MenuWorker getWorker() {
        return MenuWorker.worker;
    }

    public static class MenuWorker extends DataObjectWorker<CmsMenu> {
        private static final MenuWorker worker = new MenuWorker();

        protected MenuWorker() {
            super(CmsMenu.class);
        }

        @Override
        public CmsMenu makeFromValue(GenericValue value) throws CmsException {
            return new CmsMenu(value);
        }

        @Override
        public CmsMenu makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsMenu(delegator, fields);
        }

        @Override
        public CmsMenu findById(Delegator delegator, String id, boolean useCache) throws CmsException {
            return findById(delegator, id, useCache, null);
        }

        public CmsMenu findById(Delegator delegator, String id, boolean useCache, HttpServletRequest request) throws CmsException {
            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsMenu> cache = null;
            if (useGlobalCache) {
                cache = idCache;
            }

            String key = delegator.getDelegatorName() + "::" + id;
            CmsMenu menu = null;
            CacheEntry<CmsMenu> menuEntry = null;

            if (useGlobalCache) {
                menuEntry = cache.getEntry(key);
            }

            if (menuEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving menu from database: id: " + id + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                menu = findOne(delegator, UtilMisc.toMap("menuId", id),
                        isUseDbCacheStatic(useCache));

                if (useGlobalCache) {
                    cache.put(key, menu);
                }
            } else {
                if (menuEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving menu from cache: id: " + id + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    menu = menuEntry.getValue();
                }
            }

            return menu;
        }

        public CmsMenu findByName(Delegator delegator, String name, String webSiteId, boolean webSiteIdOptional, boolean useCache) throws CmsException {
            return findByName(delegator, name, webSiteId, webSiteIdOptional, useCache, null);
        }

        /**
         * Finds by name and optional webSiteId.
         * NOTE: if no webSiteId passed, it preferentially returns the records having no webSiteId.
         * NOTE: 2017-03-24: webSiteId IS CURRENTLY IGNORED.
         * <p>
         * TODO: WARNING: UNTESTED
         */
        public CmsMenu findByName(Delegator delegator, String name, String webSiteId, boolean webSiteIdOptional, boolean useCache, HttpServletRequest request) throws CmsException {
            // TODO: WARNING: UNTESTED
            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsMenu> cache = null;
            if (useGlobalCache) {
                cache = nameCache;
            }
            if (webSiteId != null && webSiteId.isEmpty()) {
                webSiteId = null;
            }
            String key = delegator.getDelegatorName() + "::" + name + "::" + (webSiteId != null ? webSiteId : (webSiteIdOptional ? "_OPT_" : ""));
            CmsMenu menu = null;
            CacheEntry<CmsMenu> menuEntry = null;

            if (useGlobalCache) {
                menuEntry = cache.getEntry(key);
            }

            if (menuEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving menu from database: name: " + name + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                Map<String, Object> fields = UtilMisc.toMap("menuName", name);
//                if (!webSiteIdOptional || webSiteId != null) {
//                    fields.put("webSiteId", webSiteId);
//                }
                // NOTE: always null webSiteIds first - this matters
//                List<CmsScriptTemplate> scripts = findAll(delegator, fields, UtilMisc.toList("webSiteId"), isUseDbCacheStatic(useCache));
                List<CmsMenu> menus = findAll(delegator, fields, null, isUseDbCacheStatic(useCache));
                if (menus.size() > 0) {
                    menu = menus.get(0);
                }
                if (menus.size() > 1) {
//                    if (!webSiteIdOptional || webSiteId != null) {
                    Debug.logError("Cms: Multiple menus with name '" + name + "' and webSiteId '" + webSiteId + "' found; using first found (id: " + menu.getId() + ")", module);
//                    } else if (asset.getWebSiteId() != null) {
//                        // if lookup by name only, it's usually because we expected only one result,
//                        // either one with webSiteId null (no log warning) or only one webSiteId
//                        Debug.logWarning("Cms: Multiple asset templates with name '" + name + "' and having a webSiteId found; using first found (id: " + asset.getId() + ", webSiteId: " + asset.getWebSiteId() + ")", module);
//                    }
                }

                if (useGlobalCache) {
                    cache.put(key, menu);
                }
            } else {
                if (menuEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving menu from cache: name: " + name + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    menu = menuEntry.getValue();
                }
            }

            return menu;
        }
    }

    @Override
    public void acceptEntityDepsVisitor(CmsEntityVisitor visitor, GenericValue relValue, VisitRelation relValueRelation, CmsMajorObject majorDataObj) throws Exception {
        CmsEntityVisit.acceptRelatedEntityDepsVisitor(visitor, VisitRelPlan.visitRelations, this.getEntity(), relValueRelation, relValue, this);
    }

    public static class VisitRelPlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsMenu");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                .entity("CmsMenu")
                    .self();
        }
    }
}
