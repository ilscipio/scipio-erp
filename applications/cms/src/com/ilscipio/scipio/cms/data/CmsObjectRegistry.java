package com.ilscipio.scipio.cms.data;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPage.CmsPageScriptAssoc;
import com.ilscipio.scipio.cms.content.CmsPageVersion;
import com.ilscipio.scipio.cms.control.CmsProcessMapping;
import com.ilscipio.scipio.cms.control.CmsProcessViewMapping;
import com.ilscipio.scipio.cms.control.CmsViewMapping;
import com.ilscipio.scipio.cms.data.CmsDataObject.DataObjectWorker;
import com.ilscipio.scipio.cms.menu.CmsMenu;
import com.ilscipio.scipio.cms.template.CmsAssetTemplate;
import com.ilscipio.scipio.cms.template.CmsAssetTemplate.CmsAssetTemplateScriptAssoc;
import com.ilscipio.scipio.cms.template.CmsAssetTemplateVersion;
import com.ilscipio.scipio.cms.template.CmsAttributeTemplate;
import com.ilscipio.scipio.cms.template.CmsPageTemplate;
import com.ilscipio.scipio.cms.template.CmsPageTemplate.CmsPageTemplateAssetAssoc;
import com.ilscipio.scipio.cms.template.CmsPageTemplate.CmsPageTemplateScriptAssoc;
import com.ilscipio.scipio.cms.template.CmsPageTemplateVersion;
import com.ilscipio.scipio.cms.template.CmsScriptTemplate;

/**
 * Special hardcoded registry of workers, classes and objects.
 * <p>
 * These statics MUST be stored in this separate registry class for the delayed
 * initialization, otherwise initialization problems ensue.
 */
public final class CmsObjectRegistry {

    /**
     * Maps entity names to {@link CmsDataObject.DataObjectWorker} instances.
     * Needed to be able to instantiate {@link CmsDataObject} instances generically
     * using entity values.
     */
    private static final Map<String, DataObjectWorker<?>> entityDataObjectWorkerMap;
    static {
        Map<String, DataObjectWorker<?>> map = new HashMap<>();

        // major types
        map.put("CmsMenu", CmsMenu.getWorker());
        map.put("CmsScriptTemplate", CmsScriptTemplate.getWorker());
        map.put("CmsAssetTemplate", CmsAssetTemplate.getWorker());
        map.put("CmsPageTemplate", CmsPageTemplate.getWorker());
        map.put("CmsPage", CmsPage.getWorker());
        map.put("CmsProcessMapping", CmsProcessMapping.getWorker());
        map.put("CmsViewMapping", CmsViewMapping.getWorker());

        //minor types
        map.put("CmsAssetTemplateVersion", CmsAssetTemplateVersion.getWorker());
        map.put("CmsAttributeTemplate", CmsAttributeTemplate.getWorker());
        //map.put("CmsAttributeTemplate", CmsControlDataObject.getWorker());
        map.put("CmsProcessViewMapping", CmsProcessViewMapping.getWorker());
        map.put("CmsPageTemplateAssetAssoc", CmsPageTemplateAssetAssoc.getWorker());
        map.put("CmsPageTemplateVersion", CmsPageTemplateVersion.getWorker());
        map.put("CmsPageVersion", CmsPageVersion.getWorker());
        map.put("CmsAssetTemplateScriptAssoc", CmsAssetTemplateScriptAssoc.getWorker());
        map.put("CmsPageTemplateScriptAssoc", CmsPageTemplateScriptAssoc.getWorker());
        map.put("CmsPageScriptAssoc", CmsPageScriptAssoc.getWorker());

        entityDataObjectWorkerMap = Collections.unmodifiableMap(map);
    }

    public static Map<String, DataObjectWorker<?>> getEntityDataObjectWorkerMap() {
        return entityDataObjectWorkerMap;
    }

    public static DataObjectWorker<?> getEntityDataObjectWorker(String entityName) {
        return entityDataObjectWorkerMap.get(entityName);
    }

    public static DataObjectWorker<?> getEntityDataObjectWorkerAlways(String entityName) {
        DataObjectWorker<?> res = entityDataObjectWorkerMap.get(entityName);
        if (res == null) throw new IllegalArgumentException("Unrecognized CMS entity name for data object worker: " + entityName);
        return res;
    }
}