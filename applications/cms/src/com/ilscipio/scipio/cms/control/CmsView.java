package com.ilscipio.scipio.cms.control;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;

import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.data.CmsObject;
import com.ilscipio.scipio.cms.data.CmsObjectCache;
import com.ilscipio.scipio.cms.data.CmsObjectCache.CacheEntry;

/**
 * Represents and caches view-related info.
 * <p>
 * Currently does not have an entity representation so does not extend CmsDataObject,
 * but used in similar way, namely caching.
 */
public class CmsView extends CmsObject {
    
    private static final long serialVersionUID = 1591804709705152250L;

    public static final String module = CmsView.class.getName();
    
    private static final CmsObjectCache<CmsView> nameCache = CmsObjectCache.getGlobalCache("cms.control.view.name");

//    private static final Set<String> logicalPkFieldNames = Collections.unmodifiableSet(new HashSet<String>(
//            Arrays.asList(new String[] { "webSiteId", "viewName" })
//            ));
    
    protected final Delegator delegator;
    protected final String viewName;
    protected final String webSiteId;
    
    public CmsView(Delegator delegator, String viewName, String webSiteId) {
        this.delegator = delegator;
        this.viewName = viewName;
        this.webSiteId = webSiteId;
    }
    
    /**
     * 2016: Loads ALL this page's content and products into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL, 
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY
     * (EVEN if the instance is not physically made immutable!).
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
    }

    @Override
    public boolean isPreloaded() {
        return true;
    }
    
    @Override
    public boolean isImmutable() {
        return true;
    }
    
    public String getViewName() {
        return viewName;
    }

    public String getWebSiteId() {
        return webSiteId;
    }

    public static CmsView findByName(Delegator delegator, String name, String webSiteId, boolean useCache) {
        boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
        CmsObjectCache<CmsView> cache = null;
        if (useGlobalCache) {
            cache = getNameCache();
        }
        
        String key = delegator.getDelegatorName() + "::" + webSiteId + "::" + name;
        CmsView view = null;
        CacheEntry<CmsView> viewEntry = null;
        
        if (useGlobalCache) {
            viewEntry = cache.getEntry(key);
        }

        if (viewEntry == null) {
            view = new CmsView(delegator, name, webSiteId);
            
            if (useGlobalCache) {
                cache.put(key, view);
            }
        } else {
            if (viewEntry.hasValue()) {
                if (CmsUtil.verboseOn()) {
                    Debug.logVerbose("Retrieving view from cache: " + name, module);
                }
                view = viewEntry.getValue();
            }
        }

        return view;

    }
    
    @Override
    public Delegator getDelegator() {
        return delegator;
    }
    
    public String getLogIdRepr() {
        return getLogIdRepr(webSiteId, viewName);
    }
    
    private static CmsObjectCache<CmsView> getNameCache() {
        return nameCache;
    }

    
    static String getLogIdRepr(String webSiteId, String viewName) {
        return "[view name: " + viewName + "; website: " + webSiteId + "]";
    }

    public ViewWorker getWorkerInst() {
        return ViewWorker.worker;
    }
    
    public static ViewWorker getWorker() {
        return ViewWorker.worker;
    }

    public static class ViewWorker extends ObjectWorker<CmsView> {
        private static final ViewWorker worker = new ViewWorker();
        
        protected ViewWorker() {
        }
        
        @Override
        public void clearMemoryCaches() {
            getNameCache().clear(); 
        }

    }
}
