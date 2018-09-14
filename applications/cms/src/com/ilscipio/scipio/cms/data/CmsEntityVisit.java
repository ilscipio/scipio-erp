package com.ilscipio.scipio.cms.data;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.entity.model.ModelRelation;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.content.CmsPage.VisitRelPlan;
import com.ilscipio.scipio.cms.data.CmsDataObject.DataObjectWorker;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitValueStash.MultiValueEntry;
import com.ilscipio.scipio.cms.template.CmsTemplate.TemplateBodySource;

/**
 * Entity visiting interfaces (visitor/visitee) and common implementations.
 */
public abstract class CmsEntityVisit {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String LOG_PREFIX = "Cms: Data Visit: ";

    protected CmsEntityVisit() { }

    /**
     * Interface for Visitor pattern callback visiting of CmsDataObject instances,
     * designed for use with {@link CmsMajorObject#acceptEntityDepsVisitor}.
     * <p>
     * FIXME: the parameters in this interface are insufficient for many purposes,
     * such that not enough info to track the relations between visits.
     * <p>
     * FIXME: everywhere will be passing null relationName for now. in most case
     * the default is entityName/value.getEntityName() (first param)
     */
    public interface CmsEntityVisitor {

        VisitContext getVisitContext();

        /**
         * Pre-lookup filter: Acts as a filter so visitor can decide whether to enter a value and its related values or not.
         * This is basically called as often as possible but will be best-effort.
         * Called before the value is looked up.
         * <p>
         * NOTE: this is never called for the "self" relation.
         */
        boolean shouldEnter(VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj);

        /**
         * Post-lookup filter: Acts as a filter so visitor can decide whether to enter a value and its related values or not.
         * This is basically called as often as possible but will be best-effort.
         * Called after the value is looked up.
         * This is needed to filter out MANY relation values.
         * <p>
         * NOTE: this is called for the "self" relation; in this case the relation parameter is not the SelfRelation
         * object but the true last relation to the self, unless it was null because it was the very first value, in which
         * case the SelfRelation is passed, which can be assumed to be a Major object.
         */
        boolean shouldEnter(GenericValue value, VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj);

        /**
         * Entity value visit callback. Performs both {@link #visitRecordStateOnly} and {@link #visitStateless} in one call.
         */
        public void visit(GenericValue value, VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) throws Exception;

        /**
         * Entity value visit callback that should record the value as visited WITHOUT printing it out.
         * This is used for split/delayed visit operations.
         */
        public void visitRecordOnly(GenericValue value, VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) throws Exception;

        /**
         * Entity value visit callback that should NOT modify any state, but should simply print out the data without counting it.
         * This is used for split/delayed visit operations.
         */
        public void visitWriteOnly(GenericValue value, VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) throws Exception;


        public interface VisitContext {
            Delegator getDelegator();

            /**
             * Maps an arbitrary name (usually derived from major entity name) to a VisitRelations to use.
             * This is a cache.
             * Used to support dynamic visit relation plans and easier debugging.
             * TODO?: not yet used.
             */
            Map<String, VisitRelations> getVisitRelationsDefs();

            /**
             * TODO: REVIEW: this belongs (?) in the visitor itself but I'm doing external to simplify code.
             */
            boolean isExportFilesAsTextData();

            VisitValueStash getVisitValueStash();
        }
    }


    /**
     * Base abstract visitor, provides delegator and takes care of looking up the values
     * that weren't already looked up (subclass should override this behavior to optimize).
     */
    public static abstract class AbstractCmsEntityVisitor implements CmsEntityVisitor {
        //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

        protected final Delegator delegator;
        protected final CmsEntityInfo cmsEntityInfo; // fast access
        protected boolean enterMajor = true;
        protected boolean enterContent = true;
        protected Set<String> enterEntityNames = null;
        protected Set<String> enterMajorEntityNames = null; // major entities get separate entity names filter for better control

        protected AbstractCmsEntityVisitor(Delegator delegator) {
            this.delegator = delegator;
            this.cmsEntityInfo = CmsEntityInfo.getInst(delegator);
        }

        public abstract AbstractVisitContext getVisitContext(); // subclass must implement

        public boolean isEnterMajor() { return enterMajor; }
        public void setEnterMajor(boolean enterMajor) { this.enterMajor = enterMajor; }
        public boolean isEnterContent() { return enterContent; }
        public void setEnterContent(boolean enterContent) { this.enterContent = enterContent; }
        public Set<String> getEnterEntityNames() { return enterEntityNames; }
        public void setEnterEntityNames(Set<String> enterEntityNames) { this.enterEntityNames = enterEntityNames != null ? enterEntityNames : null; }
        public Set<String> getEnterMajorEntityNames() { return enterMajorEntityNames; }
        public void setEnterMajorEntityNames(Set<String> enterMajorEntityNames) { this.enterMajorEntityNames = enterMajorEntityNames; }

        // PRE-LOOKUP FILTERS

        @Override
        public boolean shouldEnter(VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) {
            if (relation.isFilterAsMajor()) {
                if (!isEnterMajor()) {
                    return false;
                } else {
                    return isEnterMajorEntity(relation) && shouldEnterMajor(relation, relValue, majorDataObj);
                }
            } else if (relation.isFilterAsContent()) {
                if (!isEnterContent()) {
                    return false;
                } else {
                    return shouldEnterContent(relation, relValue, majorDataObj);
                }
            } else {
                return isEnterGenericEntity(relation) && shouldEnterGeneric(relation, relValue, majorDataObj);
            }
        }
        protected boolean isEnterGenericEntity(VisitRelation relation) {
            return enterEntityNames == null || enterEntityNames.contains(relation.getRelEntityName());
        }
        protected boolean isEnterMajorEntity(VisitRelation relation) {
            return enterMajorEntityNames == null || enterMajorEntityNames.contains(relation.getRelEntityName());
        }

        // methods available to subclasses to override
        protected boolean shouldEnterGeneric(VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) {
            return true;
        }
        protected boolean shouldEnterMajor(VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) {
            return true;
        }
        protected boolean shouldEnterContent(VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) {
            return true; // subclass should record visited IDs for this...
        }

        // POST-LOOKUP FILTERS, with looked-up value
        // NOTE: theoretically this should do the same as pre-lookup filter, but we're not really using them the same way

        @Override
        public boolean shouldEnter(GenericValue value, VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) {
            return true;
        }

        public abstract class AbstractVisitContext implements VisitContext {
            protected Map<String, VisitRelations> visitRelationsDefs = new HashMap<>();
            protected VisitValueStash visitValueStash = new VisitValueStash();
            @Override public Delegator getDelegator() { return delegator; }
            @Override public Map<String, VisitRelations> getVisitRelationsDefs() { return visitRelationsDefs; }
            @Override public VisitValueStash getVisitValueStash() { return visitValueStash; }
        }
    }

    public static class VisitValueStash implements Map<String, Object> {
        private Map<String, Object> stashMap;
        private List<Map<String, Object>> stashStack;
        public VisitValueStash() {
            this.stashMap = new HashMap<>();
            stashStack = new ArrayList<>();
        }
        public void reset() {
            if (CmsUtil.debugOn()) {
                if (stashMap.size() > 0) {
                    Debug.logError(LOG_PREFIX+"Reset VisitValueStash while current map still had "
                            + stashMap.size() + " entries; entity values may have been lost", module);
                } else if (stashStack.size() > 0) {
                    Debug.logError(LOG_PREFIX+"Reset VisitValueStash while current stack still had "
                            + stashStack.size() + " entries; entity values may have been lost", module);
                }
            }
            this.stashMap = new HashMap<>();
            stashStack = new ArrayList<>();
        }

        public void pushStashStack() {
            stashStack.add(stashMap);
            stashMap = new HashMap<>();
        }
        public void popStashStack() {
            if (CmsUtil.debugOn() && (stashMap.size() > 0)) {
                Debug.logError(LOG_PREFIX+"Popped VisitValueStash while current map still had "
                        + stashMap.size() + " entries; entity values may have been lost", module);
            }
            stashMap = stashStack.remove(stashStack.size() - 1);
        }

        public interface StashEntry {
        }

        /**
         * Entry that supports many values as long as they came from the same relation.
         * Will work for many relations.
         */
        public static class MultiValueEntry implements StashEntry {
            List<GenericValue> values;
            final VisitRelation relation;
            final GenericValue relValue;
            final CmsMajorObject majorDataObj;

            public MultiValueEntry(GenericValue value, VisitRelation relation, GenericValue relValue,
                    CmsMajorObject majorDataObj) {
                this.values = new ArrayList<>();
                this.values.add(value);
                this.relation = relation;
                this.relValue = relValue;
                this.majorDataObj = majorDataObj;
            }

            public List<GenericValue> getValues() { return values; }
            public VisitRelation getRelation() { return relation; }
            public GenericValue getRelValue() { return relValue; }
            public CmsMajorObject getMajorDataObj() { return majorDataObj; }

            public void addValue(GenericValue value) { this.values.add(value); }
        }

        public MultiValueEntry addMultiValueEntry(String key, GenericValue value, VisitRelation relation, GenericValue relValue,
                CmsMajorObject majorDataObj) {
            MultiValueEntry entry = (MultiValueEntry) stashMap.get(key);
            if (entry != null) {
                if (CmsUtil.debugOn()) {
                    // make sure same objects
                    if (relation != entry.getRelation() || relValue != entry.getRelValue() || majorDataObj != entry.getMajorDataObj()) {
                        Debug.logError(LOG_PREFIX+"Tried to insert into VisitValueStash adding to an existing entry"
                                + " but the relation, relValue or majorDataObject are different (key " + key + "); entity values may be corrupted", module);
                    }
                }
                entry.addValue(value);
            } else {
                entry = new MultiValueEntry(value, relation, relValue, majorDataObj);
                stashMap.put(key, entry);
            }
            return entry;
        }

        @Override public Object get(Object key) {
            return stashMap.get(key);
        }
        @Override public Object put(String key, Object value) {
            if (CmsUtil.debugOn() && containsKey(key)) {
                Debug.logError(LOG_PREFIX+"Tried to insert into VisitValueStash "
                        + "with key that already had a value: " + key + "; entity values may have been lost", module);
            }
            return stashMap.put(key, value);
        }
        @Override public Object remove(Object key) {
//            if (CmsUtil.debugOn() && !containsKey(key)) {
//                Debug.logError(LOG_PREFIX+"Tried to remove from VisitValueStash using key that has no value: " + key, module);
//            }
            return stashMap.remove(key);
        }

        @Override public int size() { return stashMap.size(); }
        @Override public boolean isEmpty() { return stashMap.isEmpty(); }
        @Override public boolean containsKey(Object key) { return stashMap.containsKey(key); }
        @Override public boolean containsValue(Object value) { return stashMap.containsValue(value); }
        @Override public void putAll(Map<? extends String, ? extends Object> m) { stashMap.putAll(m); }
        @Override public void clear() { stashMap.clear(); }
        @Override public Set<String> keySet() { return stashMap.keySet(); }
        @Override public Collection<Object> values() { return stashMap.values(); }
        @Override public Set<java.util.Map.Entry<String, Object>> entrySet() { return stashMap.entrySet(); }
    }

    public interface CmsEntityVisitee {

        /**
         * Allows a visitor (Visitor pattern) to go through the entities immediately associated
         * to the major entities in the order of dependencies of entity relations.
         * <p>
         * This generic pattern will ensure a consist visiting order for
         * all operations including export.
         * <p>
         * NOTE: the settings are all on the visitor itself.
         *
         * @param relValue the related value we are coming from
         * @param relationName related value relation name to this object FIXME: usually getting null
         * @param majorDataObj the last major data object, changes only across boundaries
         */
        public void acceptEntityDepsVisitor(CmsEntityVisitor visitor, GenericValue relValue, VisitRelation relValueRelation, CmsMajorObject majorDataObj) throws Exception;

    }

    /**
     * MAIN Generic entity relations visiting algorithm - uses the passed visitRelations visit plan
     * to go through the minor relations to the majorDataObj as well as traverse into nearby major entities.
     */
    public static void acceptRelatedEntityDepsVisitor(CmsEntityVisitor visitor,
            VisitRelations visitRelations, GenericValue value, VisitRelation relValueRelation, GenericValue relValue, CmsMajorObject majorDataObj) throws Exception {
        if (CmsUtil.verboseOn()) {
            if (!visitRelations.hasSelfEntityRelation(value.getEntityName())) {
                Debug.logInfo(LOG_PREFIX+"Note: There is no 'self' entry for (not necessarily an error): " + value.getEntityName(), module);
            }
        }

        // special top check needed for brand new queries...
        if (relValueRelation == null) {
            VisitRelation topSelfRelation = visitRelations.getSelfEntityRelation(value.getEntityName());
            if (topSelfRelation == null) {
                // NOTE: in future this case could make sense but right now all plans should an explicit self entry...
                Debug.logWarning(LOG_PREFIX+"No self relation found for top entity " + value.getEntityName()
                    + " in visit relations plan (also possible: unexpected null relValueRelation value); using dummy", module);
                topSelfRelation = new VisitRelation.SelfRelation(value.getModelEntity(), true);
            }
            if (!visitor.shouldEnter(value, topSelfRelation, relValue, majorDataObj)) {
                return;
            }
        }

        // TODO: REVIEW: not great place to push/pop stack, because severely limits scope, but just enough for now.
        visitor.getVisitContext().getVisitValueStash().pushStashStack();
        try {
            acceptRelatedEntityDepsVisitorDeep(visitor, visitRelations, value, relValueRelation, relValue, majorDataObj);
        } finally {
            visitor.getVisitContext().getVisitValueStash().popStashStack();
        }
    }

    private static void acceptRelatedEntityDepsVisitorDeep(CmsEntityVisitor visitor,
            VisitRelations visitRelations, GenericValue value, VisitRelation relValueRelation, GenericValue relValue, CmsMajorObject majorDataObj) throws Exception {

        for(VisitRelation relation : visitRelations.getEntityRelations(value.getEntityName())) {
            VisitRelation.DataRelType relType = relation.getDataRelType();
            String relationName = relation.getRelationName();

            if (!relation.isSelf() && !relation.isRecall()) { // NOTE: can't use self with pre-lookup filter
                if (!visitor.shouldEnter(relation, value, majorDataObj)) { // PRE-LOOKUP FILTER
                    continue;
                }
            }

            if (relation.isContent()) {
                acceptContentRelationEntityDepsVisitor(visitor, value, relation, majorDataObj);
            } else if (relation.isSelf()) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo(LOG_PREFIX+"Visiting entity: " + value.getEntityName() + " PK: " + value.getPkShortValueString(), module);
                }
                // NOTE: must pass relValueRelation. self relation only passed if very first in main query.
                VisitRelation selfRelation = relValueRelation != null ? relValueRelation : relation;
                if (visitor.shouldEnter(value, selfRelation, relValue, majorDataObj)) {
                    if (relation.isStash()) {
                        visitor.visitRecordOnly(value, selfRelation, relValue, majorDataObj);
                        // save the whole value in stash
                        visitor.getVisitContext().getVisitValueStash().addMultiValueEntry(relation.getStashKey(),
                                value, selfRelation, relValue, majorDataObj);
                    } else {
                        visitor.visit(value, selfRelation, relValue, majorDataObj);
                    }
                }
            } else if (relation.isRecall()) {
                VisitValueStash.MultiValueEntry stashEntry = (MultiValueEntry) visitor.getVisitContext().getVisitValueStash().remove(relation.getStashKey());
                if (stashEntry != null) {
                    for(GenericValue recallValue : stashEntry.getValues()) {
                        visitor.visitWriteOnly(recallValue, stashEntry.getRelation(), stashEntry.getRelValue(), stashEntry.getMajorDataObj());
                    }
                }
            } else if (relType.isOneAny()) {
                GenericValue newValue = value.getRelatedOne(relationName, false);
                if (newValue != null && visitor.shouldEnter(newValue, relation, value, majorDataObj)) { // POST-LOOKUP FILTER
                    if (relation.isMajor()) {
                        DataObjectWorker<?> worker = CmsObjectRegistry.getEntityDataObjectWorkerAlways(newValue.getEntityName());
                        CmsMajorObject newMajorDataObj = (CmsMajorObject) worker.makeFromValue(newValue);
                        newMajorDataObj.acceptEntityDepsVisitor(visitor, value, relation, newMajorDataObj);
                    } else {
                        acceptRelatedEntityDepsVisitorDeep(visitor, visitRelations, newValue, relation, value, majorDataObj);
                    }
                }
            } else if (relType.isMany()) {
                List<GenericValue> newValues = value.getRelated(relationName, relation.andFields, relation.orderBy, false);
                if (UtilValidate.isNotEmpty(newValues)) {
                    if (relation.isMajor()) {
                        DataObjectWorker<?> worker = CmsObjectRegistry.getEntityDataObjectWorkerAlways(relation.getRelEntityName());
                        for(GenericValue newValue : newValues) {
                            if (visitor.shouldEnter(newValue, relation, value, majorDataObj)) { // POST-LOOKUP FILTER)
                                CmsMajorObject newMajorDataObj = (CmsMajorObject) worker.makeFromValue(newValue);
                                newMajorDataObj.acceptEntityDepsVisitor(visitor, value, relation, newMajorDataObj);
                            }
                        }
                    } else {
                        for(GenericValue newValue : newValues) {
                            if (visitor.shouldEnter(newValue, relation, value, majorDataObj)) { // POST-LOOKUP FILTER)
                                acceptRelatedEntityDepsVisitorDeep(visitor, visitRelations, newValue, relation, value, majorDataObj);
                            }
                        }
                    }
                }
            } else {
                Debug.logWarning(LOG_PREFIX+"Unhandled VisitRelation type, ignoring: " + relation, module);
            }
        }
    }

    /**
     * Handles a single Content relation visit; assumes shouldEnter was already called for the Content relation.
     * NOTE: shouldEnter is not called on the records related to Content entity (we'll never need this).
     * <p>
     * FIXME: this is duplicated in {@link com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker#getContentAndRelatedValues}; both are in use.
     */
    public static void acceptContentRelationEntityDepsVisitor(CmsEntityVisitor visitor,
            GenericValue relValue, VisitRelation contentRelation, CmsMajorObject majorDataObj) throws Exception {
        Delegator delegator = visitor.getVisitContext().getDelegator();
        String contentId = contentRelation.extractRelOneEntitySingleFieldPk(relValue);
        if (UtilValidate.isNotEmpty(contentId)) {
            GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), false);
            if (content != null) {
                GenericValue dataRes = content.getRelatedOne("DataResource", false);
                if (dataRes != null) {
                    if ("ELECTRONIC_TEXT".equals(dataRes.getString("dataResourceTypeId"))) {
                        visitor.visit(dataRes, ContentVisitRelPlan.dataResRelation, relValue, majorDataObj);
                        GenericValue elecText = dataRes.getRelatedOne("ElectronicText", false);
                        if (elecText != null) {
                            visitor.visit(elecText, ContentVisitRelPlan.elecTextRelation, relValue, majorDataObj);
                        } else {
                            Debug.logError(LOG_PREFIX+"Missing ElectronicText for contentId '" + contentId + "'", module);
                        }
                    } else if (visitor.getVisitContext().isExportFilesAsTextData()) {
                        try {
                            TemplateBodySource tmplBodySrc = com.ilscipio.scipio.cms.template.CmsTemplate.getTemplateBodySourceFromContent(delegator, contentId, false);

                            dataRes.put("dataResourceTypeId", "ELECTRONIC_TEXT"); // WARN: DO NOT COMMIT THIS VALUE!
                            visitor.visit(dataRes, ContentVisitRelPlan.dataResRelation, relValue, majorDataObj);

                            GenericValue elecText = delegator.makeValue("ElectronicText",
                                UtilMisc.toMap("dataResourceId",dataRes.getString("dataResourceId"), "textData",tmplBodySrc.getEffectiveBody()));
                            visitor.visit(elecText, ContentVisitRelPlan.elecTextRelation, relValue, majorDataObj);
                        } catch(Exception e) {
                            Debug.logError(e, LOG_PREFIX+"Unable to read content body for contentId '" + contentId + "'", module);
                        }
                    } else {
                        visitor.visit(dataRes, ContentVisitRelPlan.dataResRelation, relValue, majorDataObj);
                    }
                } else {
                    Debug.logError(LOG_PREFIX+"Missing DataResource for contentId '" + contentId + "'", module);
                }
                // TODO?: in the future there may be more related records (ALTERNATE_LOCALE); not going too deep yet
                visitor.visit(content, contentRelation, relValue, majorDataObj);
            }
        }
    }

    /**
     * Visit plan for the Content entities.
     * TODO: not really used properly, hardcoded so we can dig out VisitRelation instances from it.
     */
    public static class ContentVisitRelPlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("Content");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        static final VisitRelation dataResRelation = visitRelations.getEntityRelation("Content", "DataResource");
        static final VisitRelation elecTextRelation = visitRelations.getEntityRelation("DataResource", "ElectronicText");
        public ContentVisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                .entity("Content")
                    .relationContent("DataResource", false)
                    .self()
                .entity("DataResource")
                    .self()
                    .relationContent("ElectronicText", false);
        }
    }

    /**
     * Describes the relations for a major entity.
     * Preserves builder call order.
     */
    public static class VisitRelations {
        public static final VisitRelations EMPTY = new VisitRelations(Collections.<String, Map<String, VisitRelation>>emptyMap());

        final Map<String, Map<String, VisitRelation>> entityRelMap;
        VisitRelations(Map<String, Map<String, VisitRelation>> entityRelMap) { this.entityRelMap = entityRelMap; }

        public static Builder newBuilder(Delegator delegator) {
            return Builder.newInstance(delegator);
        }
        public Builder cloneWithNewBuilder(Delegator delegator) {
            return Builder.newInstance(delegator, entityRelMap);
        }

        public Collection<VisitRelation> getEntityRelations(String entityName) {
            Map<String, VisitRelation> relMap = entityRelMap.get(entityName);
            if (relMap == null) return Collections.emptyList();
            else return relMap.values();
        }

        public VisitRelation getEntityRelation(String entityName, String relationName) {
            Map<String, VisitRelation> relMap = entityRelMap.get(entityName);
            if (relMap != null) return relMap.get(relationName);
            return null;
        }

        public boolean hasEntityRelation(String entityName, String relationName) {
            return getEntityRelation(entityName, relationName) != null;
        }

        public VisitRelation getSelfEntityRelation(String entityName) {
            return getEntityRelation(entityName, VisitRelation.SelfRelation.SELF_REL_TITLE + entityName);
        }

        public boolean hasSelfEntityRelation(String entityName) {
            return getSelfEntityRelation(entityName) != null;
        }

        /**
         * VisitRelations Builder - helps create and simplify visiting "plans" for the major entities.
         */
        public static class Builder {
            final Delegator delegator;
            final ModelReader modelReader;
            final CmsEntityInfo entityInfo;
            Map<String, Map<String, VisitRelation>> entityRelMap;
            String entityName = null;
            boolean entityIsMajor = false;
            ModelEntity modelEntity = null;
            boolean autoContent = true;

            Builder(Delegator delegator, Map<String, Map<String, VisitRelation>> entityRelMap) {
                this.delegator = delegator;
                this.modelReader = delegator.getModelReader();
                this.entityInfo = CmsEntityInfo.getInst(delegator);
                this.entityRelMap = entityRelMap;
            }

            static Builder newInstance(Delegator delegator) {
                return new Builder(EntityInfoUtil.ensureDelegator(delegator), new LinkedHashMap<String, Map<String, VisitRelation>>());
            }
            static Builder newInstance(Delegator delegator, Map<String, Map<String, VisitRelation>> entityRelMap) {
                return new Builder(EntityInfoUtil.ensureDelegator(delegator), deepCopyEntityRelMap(entityRelMap));
            }

            public static Map<String, Map<String, VisitRelation>> deepCopyEntityRelMap(Map<String, Map<String, VisitRelation>> entityRelMap) {
                Map<String, Map<String, VisitRelation>> newMap = new LinkedHashMap<>();
                if (entityRelMap != null) {
                    for(Map.Entry<String, Map<String, VisitRelation>> entry : entityRelMap.entrySet()) {
                        newMap.put(entry.getKey(), copyRelMap(entry.getValue()));
                    }
                }
                return newMap;
            }

            public static Map<String, VisitRelation> copyRelMap(Map<String, VisitRelation> relMap) {
                Map<String, VisitRelation> newMap = new LinkedHashMap<>(relMap);
                return newMap;
            }


            public Builder setAutoContent(boolean autoContent) { this.autoContent = autoContent; return this; }

            public Builder entity(String entityName, boolean entityIsMajor, Boolean autoContent) {
                // we assume the very first entity is major, the rest not
                this.entityIsMajor = entityIsMajor;
                this.entityName = entityName;
                if (autoContent != null) this.autoContent = autoContent;
                try {
                    this.modelEntity = modelReader.getModelEntity(entityName);
                } catch (GenericEntityException e) {
                    throw new IllegalArgumentException(e);
                }
                return this;
            }

            public Builder entity(String entityName) { return entity(entityName, isFirstEntity(), null); }

            public boolean isFirstEntity() { return (entityName == null && entityRelMap.isEmpty()); }

            public Builder contentAuto() {
                List<ModelRelation> relations = entityInfo.getCmsContentModelRelations(modelEntity);
                for(ModelRelation relation : relations) {
                    relation(new VisitRelation.ContentRelation(relation, true));
                    // TODO?: generate related content entities... (primaryContent=false)
                    // or hook in ContentVisitRelPlan somehow...
                }
                return this;
            }

            /**
             * Triggers a visit on the current entity (not a real relation).
             */
            public Builder self() {
                if (autoContent) contentAuto();
                return relation(new VisitRelation.SelfRelation(modelEntity, entityIsMajor));
            }

            /**
             * Triggers a record-only visit on the current entity without triggered a write
             * visit; instead of writing, the values are saved in a plan-scoped variable of the
             * given varName, which can be written out at any time using the {@link #recall} entry.
             */
            public Builder selfStash(String varName) {
                if (autoContent) contentAuto();
                return relation(new VisitRelation.SelfRelation(modelEntity, entityIsMajor, varName));
            }

            /**
             * Triggers a write-only visit of the stashed value(s) referred to by the given varName.
             */
            public Builder recall(String varName) {
                return relation(new VisitRelation.RecallRelation(modelEntity, varName));
            }

            public Builder relation(String relationName) {
                return relation(new VisitRelation.GenericRelation(getModelRelation(relationName)));
            }
            public Builder relationMajor(String relationName) {
                return relation(new VisitRelation.MajorRelation(getModelRelation(relationName)));
            }
            public Builder relationMajorNoFilter(String relationName) {
                return relation(new VisitRelation.MajorRelation(getModelRelation(relationName), false));
            }
            public Builder relationContent(String relationName, boolean primaryContent) {
                return relation(new VisitRelation.ContentRelation(getModelRelation(relationName), primaryContent));
            }
            public Builder relation(VisitRelation relation) {
                getAddRelMap(relation.getEntityName()).put(relation.getRelationName(), relation);
                return this;
            }

            private Map<String, VisitRelation> getAddRelMap(String entityName) {
                Map<String, VisitRelation> relMap = entityRelMap.get(entityName);
                if (relMap == null) {
                    relMap = new LinkedHashMap<>();
                    entityRelMap.put(entityName, relMap);
                }
                return relMap;
            }

            private Set<String> findEntityNames(boolean includeMajor, boolean includeContent) {
                Set<String> entityNames = new HashSet<>(entityRelMap.keySet());
                for(Map<String, VisitRelation> relations : entityRelMap.values()) {
                    for(VisitRelation relation : relations.values()) {
                        if (relation.isMajor()) {
                            if (includeMajor) entityNames.add(relation.getRelEntityName());
                        } else if (relation.isContent()) {
                            if (includeContent) entityNames.add(relation.getRelEntityName());
                        } else {
                            entityNames.add(relation.getRelEntityName());
                        }
                    }
                }
                return entityNames;
            }

            private Set<String> findGenericEntityNames() {
                return findEntityNames(false, false);
            }

            /**
             * creates auto self entries because they'll be too slow to instantiate at run time.
             * always called on complete().
             */
            private Builder autoGenSelfEntries() {
                for(String entityName : findGenericEntityNames()) {
                    String selfRelName = VisitRelation.SelfRelation.SELF_REL_TITLE + entityName;
                    Map<String, VisitRelation> relMap = getAddRelMap(entityName);
                    if (relMap.size() == 0) {
                        relMap.put(selfRelName, makeSelfRelation(entityName, false)); // can't make major self here, shouldn't be need
                    } else if (!relMap.containsKey(selfRelName)) {
                        // no, allow caller to do what he wants
//                        Debug.logWarning(LOG_PREFIX+"Entity " + entityName + " visit relations map is missing an explicit self() entry", module);
//                        Map<String, VisitRelation> newRelMap = new LinkedHashMap<>();
//                        // add as first
//                        newRelMap.put(relationName, makeSelfRelation(entityName));
//                        newRelMap.putAll(relMap);
//                        this.entityRelMap.put(entityName, newRelMap);
                        if (CmsUtil.verboseOn()) {
                            Debug.logInfo(LOG_PREFIX+"Entity " + entityName + " visit relations map has no self() entry (not necessarily an error)", module);
                        }
                    }
                }
                return this;
            }

            private VisitRelation makeSelfRelation(String entityName, boolean major) {
                try {
                    ModelEntity modelEntity = modelReader.getModelEntity(entityName);
                    return new VisitRelation.SelfRelation(modelEntity, major);
                } catch (GenericEntityException e) {
                    throw new IllegalArgumentException(e);
                }
            }

            public Builder removeRelation(String entityName, String relationName) {
                Map<String, VisitRelation> relMap = entityRelMap.get(entityName);
                if (relMap != null) {
                    relMap.remove(relationName);
                    if (relMap.isEmpty()) {
                        entityRelMap.remove(entityName);
                    }
                }
                return this;
            }
            public Builder removeAllRelationsOfType(boolean self, boolean major, boolean content) {
                for(String entityName : new ArrayList<>(entityRelMap.keySet())) {
                    Map<String, VisitRelation> relMap = entityRelMap.get(entityName);
                    for(VisitRelation relation : new ArrayList<>(relMap.values())) {
                        if ((self && relation.isSelf()) || (major && relation.isMajor() || (content && relation.isContent()))) {
                            relMap.remove(relation.getRelationName());
                            if (relMap.isEmpty()) {
                                entityRelMap.remove(entityName);
                            }
                        }
                    }
                }
                return this;
            }
            public Builder removeAllMajorRelations() {
                return removeAllRelationsOfType(false, true, false);
            }
            public Builder removeAllContentRelations() {
                return removeAllRelationsOfType(false, false, true);
            }
            public Builder removeAllRelationsOfRelEntityName(String relEntityName) {
                for(String entityName : new ArrayList<>(entityRelMap.keySet())) {
                    Map<String, VisitRelation> relMap = entityRelMap.get(entityName);
                    for(VisitRelation relation : new ArrayList<>(relMap.values())) {
                        if (relEntityName.equals(relation.getRelEntityName())) {
                            relMap.remove(relation.getRelationName());
                            if (relMap.isEmpty()) {
                                entityRelMap.remove(entityName);
                            }
                        }
                    }
                }
                return this;
            }

            public VisitRelations complete() {
                autoGenSelfEntries();
                return new VisitRelations(entityRelMap);
            }

            private ModelRelation getModelRelation(String relationName) {
                ModelRelation modelRelation = modelEntity.getRelation(relationName);
                if (modelRelation == null) throw new IllegalArgumentException("No relation with name '" + relationName + "' exists on entity '" + entityName + "'");
                return modelRelation;
            }
        }

        /**
         * Wraps a {@link VisitRelations#Builder} invocations to handle errors.
         */
        public static abstract class BuildPlan {
            private final String majorEntityName;
            private final BuildPlan fallbackPlan; // allows debug and other screens to keep working

            public BuildPlan(String majorEntityName, BuildPlan fallbackPlan) { this.majorEntityName = majorEntityName; this.fallbackPlan = fallbackPlan; }
            public BuildPlan(String majorEntityName) { this(majorEntityName, new SelfOnlyBuildPlan(majorEntityName)); }

            public String getMajorEntityName() { return majorEntityName; }
            public BuildPlan getFallbackPlan() { return fallbackPlan; }

            public final VisitRelations buildSafe() { return buildSafe(null); }
            public final VisitRelations buildSafe(Delegator delegator) {
                VisitRelations visitRelations;
                try {
                    visitRelations = build(delegator);
                } catch(Exception e) {
                    Debug.logError(e, LOG_PREFIX+"Error constructing visit relations plan " + this + ": " + e.getMessage(), module);
                    BuildPlan fallbackPlan = getFallbackPlan();
                    if (fallbackPlan != null) {
                        try {
                            visitRelations = fallbackPlan.build(delegator);
                        } catch(Exception ef) {
                            Debug.logError(ef, LOG_PREFIX+"Error constructing visit relations fallback plan " + fallbackPlan + ": " + e.getMessage(), module);
                            visitRelations = VisitRelations.EMPTY;
                        }
                    } else {
                        visitRelations = VisitRelations.EMPTY;
                    }
                }
                return visitRelations;
            }
            public final VisitRelations build(Delegator delegator) throws Exception {
                delegator = EntityInfoUtil.ensureDelegator(delegator);
                return planDefinition(delegator).complete();
            }
            protected VisitRelations.Builder newBuilder(Delegator delegator) { return VisitRelations.newBuilder(delegator); }
            /**
             * Provides the plan definition, in the form of a new {@link VisitRelations#Builder}.
             */
            protected abstract VisitRelations.Builder planDefinition(Delegator delegator) throws Exception;

            @Override
            public String toString() { return "[(BuildPlan) major entity: " + getMajorEntityName() + "]"; }

            public static class SelfOnlyBuildPlan extends BuildPlan {
                public SelfOnlyBuildPlan(String majorEntityName) { super(majorEntityName, null); }
                @Override
                public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
                    return newBuilder(delegator).entity(getMajorEntityName()).self();
                }
                @Override
                public String toString() {
                    return "[(BuildPlan) major entity: " + getMajorEntityName() + ", self-only]";
                }
            }
        }
    }

    /**
     * Visit relation entry, belonging to an entity. Part of {@link VisitRelations} structure.
     * NOTE: this has evolved and some subclasses are not really relations at all, though most are.
     * TODO: find better design.
     */
    public static abstract class VisitRelation {
        /**
         * Special value used as Title in "self" relations; gets prefixed to the entity
         * name to give full relationName (combined name).
         */
        public static final String SELF_REL_TITLE = "SELF_";
        /**
         * Special value used as Title prefix in "recall" relations; appended to this prefix
         * is the recall variable name followed by the entity name.
         */
        public static final String RECALL_REL_TITLE_PREFIX = "RECALL_";

        protected final ModelRelation modelRelation;
        protected final String entityName;
        protected final String relationName;
        protected final DataRelType dataRelType;
        protected final Map<String, Object> andFields;
        protected final List<String> orderBy;

        /**
         * Data relation type, superset of the ofbiz entity relation types.
         */
        public enum DataRelType {
            SELF("self"), // WARN: type str not recognized by ofbiz classes (works by ModelRelation lenience only)
            RECALL("recall"), // WARN: type str not recognized by ofbiz classes (works by ModelRelation lenience only)
            ONE("one"),
            ONE_NOFK("one-nofk"),
            MANY("many");

            private static final Map<String, DataRelType> typeStrMap;
            static {
                Map<String, DataRelType> map = new HashMap<>();
                for(DataRelType type : DataRelType.values()) {
                    map.put(type.getTypeStr(), type);
                }
                typeStrMap = map;
            }

            private final String typeStr;

            private DataRelType(String typeStr) {
                this.typeStr = typeStr;
            }
            public String getTypeStr() {
                return typeStr;
            }
            public boolean isSelf() { return this == SELF; }
            public boolean isOneAny() { return this == ONE || this == ONE_NOFK; }
            public boolean isMany() { return this == MANY; }
            public boolean isRecall() { return this == RECALL; }

            public static DataRelType fromTypeStr(String typeStr) {
                return typeStrMap.get(typeStr);
            }

            public static DataRelType fromTypeStrAlways(String typeStr) throws IllegalArgumentException {
                DataRelType type = typeStrMap.get(typeStr);
                if (type == null) throw new IllegalArgumentException("Invalid or missing entity relation type: " + typeStr);
                return type;
            }
        }

        protected VisitRelation(ModelRelation modelRelation, String entityName, String relationName, DataRelType type,
                Map<String, Object> andFields, List<String> orderBy) {
            this.entityName = entityName;
            this.relationName = relationName;
            this.modelRelation = modelRelation;
            this.dataRelType = type;
            this.andFields = andFields;
            this.orderBy = orderBy;
        }

        protected VisitRelation() {
            this(null, null, null, null, null, null);
        }

        protected VisitRelation(ModelRelation modelRelation, Map<String, Object> andFields, List<String> orderBy) {
            this(modelRelation, modelRelation.getModelEntity().getEntityName(),
                    modelRelation.getCombinedName(), DataRelType.fromTypeStrAlways(modelRelation.getType()), andFields, orderBy);
        }

        protected VisitRelation(ModelRelation modelRelation) {
            this(modelRelation, null, null);
        }

        public String getEntityName() { return entityName; }
        public String getRelationName() { return relationName; }
        public String getRelEntityName() { return modelRelation.getRelEntityName(); }
        public ModelRelation getModelRelation() { return modelRelation; }
        public DataRelType getDataRelType() { return dataRelType; }
        public Map<String, Object> getAndFields() { return andFields; }
        public List<String> getOrderBy() { return orderBy; }

        public boolean isSelf() { return false; }
        public boolean isMajor() { return false; }
        /**
         * If set to false, allows a Major entity to bypass the major entity names filter when crossing
         * into a new entity (see {@link AbstractCmsEntityVisitor#shouldEnter(VisitRelation, GenericValue, CmsMajorObject)}).
         * In such case, the Generic entity names filter applies to it instead.
         * In all other cases, it is still treated as a Major entity.
         */
        public boolean isFilterAsMajor() { return isMajor(); }
        public boolean isContent() { return false; }
        public boolean isFilterAsContent() { return isContent(); }
        public boolean isContentPrimary() { return false; }
        public boolean isStash() { return false; }
        public String getStashKey() { return null; }
        public boolean isRecall() { return false; }

        public String getRelationPkSingleFieldName() { return modelRelation.getKeyMaps().get(0).getFieldName(); }

        public String extractRelOneEntitySingleFieldPk(GenericValue relValue) {
            if (CmsUtil.verboseOn()) {
                if (!getDataRelType().isOneAny()) throw new IllegalStateException("Tried to get single PK on non-one relation type for relation " + this.toString());
                if (modelRelation.getKeyMaps().size() != 1) throw new IllegalStateException("Tried to get single PK on multi-field-pk entity for relation " + this.toString());
            }
            return relValue.getString(getRelationPkSingleFieldName());
        }

        @Override
        public String toString() {
            return "[(VisitRelation) entityName: " + getEntityName()
                + ", relationName: " + getRelationName()
                + ", dataRelType: " + getDataRelType().getTypeStr()
                + ", major: " + isMajor()
                + ", content: " + isContent()
                + "]";
        }

        // NOTE: this class hierarchy might be removed later, don't count on it being here...

        static class GenericRelation extends VisitRelation {
            public GenericRelation(ModelRelation modelRelation) { super(modelRelation); }
        }

        static class SelfRelation extends VisitRelation {
            protected final boolean major;
            protected final String stashVarName;
            public SelfRelation(ModelEntity modelEntity, boolean major, String stashVarName) {
                super(createSelfModelRelation(modelEntity), modelEntity.getEntityName(), SELF_REL_TITLE + modelEntity.getEntityName(), DataRelType.SELF, null, null);
                this.major = major;
                this.stashVarName = stashVarName;
            }
            public SelfRelation(ModelEntity modelEntity, boolean major) { this(modelEntity, major, null); }

            @Override public boolean isSelf() { return true; }
            @Override public boolean isMajor() { return major; }
            @Override public String getRelEntityName() { return entityName; }
            @Override public boolean isStash() { return stashVarName != null; }
            @Override public String getStashKey() { return stashVarName; }
        }
        static ModelRelation createSelfModelRelation(ModelEntity modelEntity) {
            // WARN: type str not recognized by ofbiz classes
            return ModelRelation.create(modelEntity, "Scipio special self-ref entity for entity visit", DataRelType.SELF.getTypeStr(), SELF_REL_TITLE,
                    modelEntity.getEntityName(), null, null, true);
        }

        static class RecallRelation extends VisitRelation {
            protected final String varName;
            public RecallRelation(ModelEntity modelEntity, String varName) {
                super(createSelfModelRelation(modelEntity), modelEntity.getEntityName(),
                        makeRecallRelTitle(varName) + modelEntity.getEntityName(), DataRelType.RECALL, null, null);
                this.varName = varName;
            }
            @Override public boolean isRecall() { return true; }
            @Override public String getStashKey() { return varName; }
        }
        static String makeRecallRelTitle(String varName) { return RECALL_REL_TITLE_PREFIX + varName + "_"; }
        static ModelRelation createRecallModelRelation(ModelEntity modelEntity, String varName) {
            // WARN: type str not recognized by ofbiz classes
            return ModelRelation.create(modelEntity, "Scipio special recall-ref entity for entity visit", DataRelType.RECALL.getTypeStr(), makeRecallRelTitle(varName),
                    modelEntity.getEntityName(), null, null, true);
        }

        static class MajorRelation extends VisitRelation {
            protected final boolean filterAsMajor;
            public MajorRelation(ModelRelation modelRelation, boolean majorFilter) { super(modelRelation); this.filterAsMajor = majorFilter; }
            public MajorRelation(ModelRelation modelRelation) { this(modelRelation, true); }
            @Override public boolean isMajor() { return true; }
            @Override public boolean isFilterAsMajor() { return filterAsMajor; }
        }

        static class ContentRelation extends VisitRelation {
            protected final boolean primary;
            public ContentRelation(ModelRelation modelRelation, boolean primary) { super(modelRelation); this.primary = primary;}
            @Override public boolean isContent() { return true; }
            @Override public boolean isContentPrimary() { return primary; }
        }
    }


    @SuppressWarnings("serial")
    public static class VisitException extends CmsException {
        public VisitException(PropertyMessage propMsg, Throwable e) { super(propMsg, e); }
        public VisitException(PropertyMessage propMsg) { super(propMsg); }
        public VisitException(String msg, PropertyMessage propMsg, Throwable e) { super(msg, propMsg, e); }
        public VisitException(String msg, PropertyMessage propMsg) { super(msg, propMsg); }
        public VisitException(String msg, Throwable e) { super(msg, e); }
        public VisitException(String msg) { super(msg); }
        public VisitException(Throwable e) { super(e); }
    }
}
