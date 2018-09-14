package com.ilscipio.scipio.cms.template;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.template.CmsMasterComplexTemplate.CmsTemplateScriptAssoc.TemplateScriptAssocWorker;
import com.ilscipio.scipio.cms.template.CmsScriptTemplate.CmsScriptTemplateAssoc;

/**
 * Master complex template - basically asset or page template.
 * <p>
 * Factors out attached script execution support, implies version support.
 * <p>
 * NOTE: this is poorly defined due to the inheritance, can clean up later.
 */
public abstract class CmsMasterComplexTemplate<T extends CmsVersionedComplexTemplate<T, V>, V extends CmsTemplateVersion> extends CmsVersionedComplexTemplate<T, V> {

    private static final long serialVersionUID = -5341063606131700832L;

    protected static final boolean removeAssociatedScripts = UtilProperties.getPropertyAsBoolean("cms", "page.template.remove.removeAssociatedScripts", true);

    /**
     * A list of script templates (assocs) sorted by inputPosition.
     * <p>
     * WARN: because we store CmsScriptTemplate and not CmsScriptTemplateAssoc, this has implications
     * for preloadContent().
     */
    protected List<CmsScriptTemplate> sortedScriptTemplates;

    protected CmsMasterComplexTemplate(GenericValue entity) {
        super(entity);
    }

    public CmsMasterComplexTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, fields);
    }

    protected CmsMasterComplexTemplate(CmsMasterComplexTemplate<T, V> other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
        this.sortedScriptTemplates = copyScriptTemplateAssocs(other.getSortedScriptTemplates(), copyArgs);
    }

    /**
     * Creates copies of the CmsScriptTemplateAssoc, returned nested inside the CmsScriptTemplate instances.
     * NOTE: this is inefficient, but by reusing sortedScriptTemplates
     * we can avoid adding an extra field that then ends up in the live global cache.
     */
    public static List<CmsScriptTemplate> copyScriptTemplateAssocs(List<CmsScriptTemplate> otherScripts, Map<String, Object> copyArgs) {
        List<CmsScriptTemplate> scripts = new ArrayList<>(otherScripts.size());
        for(CmsScriptTemplate otherScript : otherScripts) {
            CmsScriptTemplateAssoc otherAssoc = otherScript.getAssoc();
            // this should never happen...
            if (otherAssoc == null) throw new CmsException("internal error: source template's script instances are missing associations");

            CmsScriptTemplateAssoc newAssoc = otherAssoc.copy(copyArgs);
            newAssoc.clearTemplate(); // will get updated on store
            CmsScriptTemplate script = new CmsScriptTemplate(otherScript.getEntity(), newAssoc);
            scripts.add(script);
        }
        return scripts;
    }

    @Override
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(fields, setIfEmpty);
    }

    /**
     * 2016: Loads ALL this object's content into the current instance.
     * <p>
     * WARN: IMPORTANT: AFTER THIS CALL,
     * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY.
     * Essential for thread safety!!!
     */
    @Override
    public void preload(PreloadWorker preloadWorker) {
        super.preload(preloadWorker);
        this.sortedScriptTemplates = preloadWorker.preloadDeep(this.getSortedScriptTemplates());
    }

    /**
     * Skip preload of the active version if have a local activeContentId.
     */
    @Override
    protected boolean hasLocalActiveTemplateBodySource() {
        return UtilValidate.isNotEmpty(getActiveTemplateContentId());
    }

    @Override
    public TemplateBodySource getTemplateBodySource() {
        TemplateBodySource tmplBodySrc = this.tmplBodySrc;
        if (tmplBodySrc == null) {
            if (hasLocalActiveTemplateBodySource()) {
                return getLocalActiveTemplateBodySource();
            } else {
                return super.getTemplateBodySource();
            }
        }
        return tmplBodySrc;
    }


    /**
     * Gets body source from local activeContentId instead of going through active version.
     */
    public TemplateBodySource getLocalActiveTemplateBodySource() {
        TemplateBodySource tmplBodySrc = this.tmplBodySrc;
        if (tmplBodySrc == null) {
            try {
                tmplBodySrc = getTemplateBodySourceFromContent(getDelegator(), getActiveTemplateContentId(), false);
            } catch (CmsException e) {
                throw new CmsException("Could not get template body for template " + getId() + " of " + getEntityName(), e);
            }
            this.tmplBodySrc = tmplBodySrc;
        }
        return tmplBodySrc;
    }

    @Override
    public V getActiveVersion() {
        return super.getActiveVersion();
    }


    /**
     * Returns a list of sorted script templates linked to this page template.
     */
    public List<CmsScriptTemplate> getSortedScriptTemplates() {
        List<CmsScriptTemplate> sortedScriptTemplates = this.sortedScriptTemplates;
        if (sortedScriptTemplates == null) {
            try {
                sortedScriptTemplates = readSortedScriptTemplates(entity, getTemplateScriptAssocWorker());
            } catch (Exception e) {
                throw new CmsException("Script templates could not be retrieved for template: " + getName(), e);
            }
            this.sortedScriptTemplates = sortedScriptTemplates;
        }
        return sortedScriptTemplates;
    }

    public static List<CmsScriptTemplate> readSortedScriptTemplates(GenericValue entity, TemplateScriptAssocWorker<?> assocWorker) throws GenericEntityException {
        List<CmsScriptTemplate> sortedScriptTemplates;
        // NOTE: nulls first corresponds to default value 0
        List<GenericValue> assocEntities = entity.getRelated(assocWorker.getEntityName(), null,
                UtilMisc.toList("inputPosition ASC NULLS FIRST"), false);
        if (assocEntities.size() > 0) {
            ArrayList<CmsScriptTemplate> sortedScriptTmplArr = new ArrayList<>(assocEntities.size());
            for(GenericValue assocEntity : assocEntities) {
                CmsTemplateScriptAssoc assoc = assocWorker.makeFromValue(assocEntity);
                sortedScriptTmplArr.add(assoc.getScriptTemplate());
            }
            sortedScriptTmplArr.trimToSize();
            sortedScriptTemplates = sortedScriptTmplArr;
        } else {
            sortedScriptTemplates = Collections.emptyList();
        }
        return sortedScriptTemplates;
    }

    public List<CmsScriptTemplate> getScriptTemplates() {
        return getSortedScriptTemplates();
    }

    @Override
    public void store() throws CmsException {
        super.store();
        checkStoreScriptTemplateAssocs(this, this.sortedScriptTemplates);
    }

    /**
     * Check if the given templates need a template and a store call and stores.
     * This is usually triggered by copy() operation, because creating new assocs
     * is done independently from the template (currently).
     */
    public static void checkStoreScriptTemplateAssocs(CmsDataObject template, List<CmsScriptTemplate> scriptTemplates) {
        if (UtilValidate.isNotEmpty(scriptTemplates)) {
            // store the associations, updating the template if has none
            for(CmsScriptTemplate script : scriptTemplates) {
                CmsScriptTemplateAssoc assoc = script.getAssoc();
                if (assoc != null && !assoc.hasTemplate()) {
                    assoc.setTemplate(template);
                    assoc.store();
                }
            }
        }
    }

    @Override
    protected void updateStoreLocalActiveContent(String activeContentId) {
        String prevContentId = getActiveTemplateContentId();
        String nextContentId;
        if (activeContentId != null) {
            nextContentId = activeContentId.isEmpty() ? null : activeContentId;
        } else {
            V activeVersion = readActiveVersion(); // bypass local cache, to be sure
            if (activeVersion != null) {
                nextContentId = activeVersion.getTemplateContentId();
            } else {
                nextContentId = null;
            }
        }
        if (!StringUtils.equals(nextContentId, prevContentId)) {
            this.setActiveTemplateContentId(nextContentId);
            this.storeSelfOnly();
        }
    }

    /**
     * Sets the local field activeContentId IF supported, which is a cached optimized
     * lookup for live renders.
     */
    public void setActiveTemplateContentId(String activeContentId) {
        entity.setString("activeContentId", activeContentId);
    }

    /**
     * Returns the local field activeContentId IF supported, which is a cached optimized
     * lookup for live renders.
     */
    public String getActiveTemplateContentId() {
        return entity.getString("activeContentId");
    }

    @Override
    public int remove() throws CmsException {
        int rowsAffected = 0;

        try {
            rowsAffected += removeScriptTemplates(entity, getTemplateScriptAssocWorker());
        } catch (GenericEntityException e) {
            throw makeRemoveException(e);
        }

        return super.remove() + rowsAffected;
    }

    public static int removeScriptTemplates(GenericValue entity, TemplateScriptAssocWorker<?> assocWorker) throws GenericEntityException {
        int rowsAffected = 0;
        // delete CmsXxxScriptAssoc (and CmsScriptTemplate if configured to do so)
        List<GenericValue> scriptAssoc = entity.getRelated(assocWorker.getEntityName(), null, null, false);
        for (GenericValue scriptValue : scriptAssoc) {
            GenericValue targetScript = scriptValue.getRelatedOne("CmsScriptTemplate", false);

            scriptValue.remove();
            rowsAffected += 1;

            if (targetScript != null) {
                CmsScriptTemplate scriptTemplate = CmsScriptTemplate.getWorker().makeFromValue(targetScript);
                rowsAffected += scriptTemplate.removeIfOrphan();
            }
        }
        return rowsAffected;
    }

    protected abstract CmsTemplateScriptAssoc.TemplateScriptAssocWorker<?> getTemplateScriptAssocWorker();

    /**
     * NOTE: Do not confuse for CmsScriptTemplateAssoc (!).
     */
    public static abstract class CmsTemplateScriptAssoc extends CmsScriptTemplate.CmsScriptTemplateAssoc {

        private static final long serialVersionUID = -3241643621064402306L;

        protected CmsTemplateScriptAssoc(GenericValue entity) {
            super(entity);
        }

        public CmsTemplateScriptAssoc(Delegator delegator, Map<String, ?> fields, CmsScriptTemplate scriptTemplate) {
            super(delegator, fields, scriptTemplate);
        }

        protected CmsTemplateScriptAssoc(CmsTemplateScriptAssoc other, Map<String, Object> copyArgs) {
            super(other, copyArgs);
            // NOTE: don't bother clearing out the ID fields here, caller should handle
        }

        @Override
        public void update(Map<String, ?> fields, boolean setIfEmpty) {
            super.update(fields, setIfEmpty);
        }

        /**
         * 2016: Loads ALL this object's content into the current instance.
         * <p>
         * WARN: IMPORTANT: AFTER THIS CALL,
         * NO FURTHER CALLS ARE ALLOWED TO MODIFY THE INSTANCE IN MEMORY.
         * Essential for thread safety!!!
         */
        @Override
        public void preload(PreloadWorker preloadWorker) {
            super.preload(preloadWorker);
        }

        public static abstract class TemplateScriptAssocWorker<T extends CmsTemplateScriptAssoc> extends ScriptTemplateAssocWorker<T> {

            protected TemplateScriptAssocWorker(Class<T> dataObjectClass) {
                super(dataObjectClass);
            }

            public T createUpdateScriptTemplateAndAssoc(Delegator delegator, Map<String, ?> origFields, GenericValue userLogin) {
                T scriptAssoc;
                Map<String, Object> fields = new HashMap<>(origFields);
                fields.put("lastUpdatedBy", userLogin.get("userLoginId"));

                String scriptAssocId = (String) fields.get("scriptAssocId");
                if (UtilValidate.isNotEmpty(scriptAssocId)) {
                    scriptAssoc = findByIdAlways(delegator, scriptAssocId, false);
                    scriptAssoc.update(fields, true);

                    CmsScriptTemplate scriptTemplate = scriptAssoc.getScriptTemplate();
                    if (scriptTemplate == null) {
                        throw new CmsException(getEntityName() + " scriptAssocId '" + scriptAssocId + "' is missing scriptTemplateId");
                    }
                    scriptTemplate.update(fields);
                } else {
                    // Only create a new CmsScriptTemplate if also creating a new assoc. existing assoc should always have existing script template.
                    // NOTE: store=false; done by scriptAssoc.store()
                    CmsScriptTemplate scriptTemplate = CmsScriptTemplate.createUpdateScriptTemplate(delegator, fields, userLogin, false);

                    scriptAssoc = makeFromFields(delegator, fields, scriptTemplate);
                }

                scriptAssoc.store();
                return scriptAssoc;
            }

            protected abstract T makeFromFields(Delegator delegator, Map<String, ?> fields, CmsScriptTemplate scriptTemplate) throws CmsException;

        }
    }
}
