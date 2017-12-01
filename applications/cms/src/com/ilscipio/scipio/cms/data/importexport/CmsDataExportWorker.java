/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package com.ilscipio.scipio.cms.data.importexport;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.Serializable;
import java.io.StringWriter;
import java.io.Writer;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.transaction.Transaction;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.data.SpecDataResEntityInfo;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityComparisonOperator;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityJoinOperator;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelViewEntity;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityFindOptions;
import org.ofbiz.entity.util.EntityListIterator;
import org.ofbiz.security.Security;

import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.data.CmsDataObject.DataObjectWorker;
import com.ilscipio.scipio.cms.data.CmsEntityInfo;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.data.CmsObjectRegistry;
import com.ilscipio.scipio.cms.data.EntityInfoUtil;
import com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker.PresetConfig;
import com.ilscipio.scipio.cms.media.CmsMediaWorker;
import com.ilscipio.scipio.cms.template.CmsTemplate.TemplateBodySource;

/**
 * CMS data export worker.
 * <p>
 * Holds both parameters and state.
 * NOT thread-safe; use {@link #cloneWorkerNewState} before run when passed across session.
 * <p>
 * NOTE: Code here was migrated from CmsDataExport.groovy and CmsDataExportRaw.jsp progressively.
 * It contains several levels and the lower ones may not make that much sense to use anymore.
 */
@SuppressWarnings("serial")
public abstract class CmsDataExportWorker implements Serializable {

    public static final String module = CmsDataExportWorker.class.getName();
    
    protected static final int DEFAULT_TRANSACTION_TIMEOUT = 3600;
    protected static final int LOG_RECORD_COUNT_INTERVAL = 500;
    
    private static final List<String> extEntityOrder = UtilMisc.unmodifiableArrayList("DataResource", "ElectronicText", "Content");
    
    public static final String LOG_PREFIX = "Cms: Data Export: ";
    
    /******************************************************/
    /* Essential/Configuration Variables */
    /******************************************************/
    // NOTE: See cmsExportData* service interfaces for descriptions of worker options
    
    protected final Delegator delegator;
    // DEV NOTE: final could be removed on some of these, using to prevent errors
    protected final CmsEntityInfo entityInfo;
    protected final boolean useTrans;
    protected final boolean newTrans;
    protected final int transTimeout;
    protected final Map<String, Set<String>> entityCdataFields;
    protected final boolean exportFilesAsTextData;
    protected final Set<String> targetEntityNames; // NOTE: this gets reordered
    protected final Set<String> targetSpecialEntityNames;
    protected final Set<String> targetCombinedEntityNames; // targetEntityNames + targetSpecialEntityNames
    protected final EntityCondition entityCond;
    protected final EntityCondition entityDateCond;
    protected final Map<String, EntityCondition> entityCondMap; // NOTE: this gets reordered
    protected final boolean includeContentRefs;
    protected EntityFindOptions mainEfo;
    protected final String attribTmplAssocType; // NOTE: this is pre-included in entityCondMap
    protected final String pmpsMappingTypeId; // NOTE: this is pre-included in entityCondMap
    protected final boolean doSpecialProcessViewMappingFilter;
    protected final boolean mediaExportVariants;
    
    /******************************************************/
    /* State Variables */
    /******************************************************/

    private Set<String> seenContentIds = new LinkedHashSet<>(); // SCIPIO: prevent duplicate outs and allows delayed output when recordGroupingByType
    private Set<String> seenCmsProcessMappingIds = new LinkedHashSet<>(); // using with doSpecialProcessViewMappingFilter
    private ModelEntity currentRecordModel = null;
    private String currentRecordName = null; 
    private Set<String> currentContentIdFieldNames = null;
    private PrintWriter writer = null;
    
    
    /******************************************************/
    /* Constructors */
    /******************************************************/
    
    /**
     * Base common worker args.
     * NOTE: See cmsExportData* service interfaces for descriptions of worker options
     * <p>
     * Initialized from defaults and/or presets. 
     * The last assignment to a field gets priority, unless null was passed in which case ignored;
     * this is how defaults are handled (slow but clear).
     * <p>
     * NOTE: 2017-06-02: currently all the options fit in here.
     * In case they don't is the reason for <T> which allows "return (T) this;" for one free level of subclassing later.
     */
    @SuppressWarnings("unchecked")
    public static class CommonWorkerArgs<T extends CommonWorkerArgs<T>> { 
        // context
        final Delegator delegator;
        final CmsEntityInfo entityInfo;
        
        PresetConfig presetConfig = null; // currently this is only the last applied preset
        boolean useTrans = true;
        boolean newTrans = false;
        int transTimeout = DEFAULT_TRANSACTION_TIMEOUT;
        
        // general options
        Collection<String> targetEntityNames;
        Collection<String> enterEntityNames; // used in Object grouping mode only
        Collection<String> enterMajorEntityNames; // used in Object grouping mode only
        boolean includeContentRefs = true; // this is almost universally needed
        boolean exportFilesAsTextData = false; // makes extremely large files
        EntityCondition entityCond = null;
        EntityCondition entityDateCond = null;
        Map<String, EntityCondition> entityCondMap = new HashMap<>(); // NOTE: only affects the main queries, not the slave queries in Object grouping mode
        EntityFindOptions mainEfo;
        
        // mode-specific options
        RecordGrouping recordGrouping = null; // default would depend on file modes...
        boolean includeMajorDeps = false;
        int maxRecordsPerFile = 0;

        // extra convenience fields
        // NOTE: empty string is significant for overriding defaults
        String attribTmplAssocType = null; // NOTE: when the args are finalized, this is simply merged as conditions into entityCondMap (convenience option)
        String pmpsMappingTypeId = null; // NOTE: when the args are finalized, this is simply merged as conditions into entityCondMap (convenience option)

        Boolean mediaExportVariants = null;
        
        public CommonWorkerArgs(Delegator delegator) { 
            this.delegator = delegator;
            this.entityInfo = CmsEntityInfo.getInst(this.delegator);
        }
        
        public T applyPreset(PresetConfig preset) {
            if (preset == null) return (T) this;
            this.presetConfig = preset;
            // apply the values (if non-null/non-empty)
            setTargetEntityNames(preset.getEntityNames());
            setAttribTmplAssocType(preset.getAttribTmplAssocType());
            setPmpsMappingTypeId(preset.getPmpsMappingTypeId());
            return (T) this;
        }
        public T applyPreset(String presetName) { return applyPreset(EntityPresetMap.getInst(delegator).get(presetName)); }

        /**
         * this is used to apply the entity names from the preset to the lateral entity traversing instead
         * of the bulk main queries. In order for this to take effect, 
         */
        protected void setEnterEntityNamesFromPresetInternal(PresetConfig preset) { 
            if (preset != null) {
                this.enterEntityNames = preset.getEntityNames();
            }
        }
        
        /**
         * this is used to apply the entity names from the preset to the lateral entity traversing instead
         * of the bulk main queries. In order for this to take effect, 
         */
        protected void setEnterMajorEntityNamesFromPresetInternal(PresetConfig preset) { 
            if (preset != null) {
                this.enterMajorEntityNames = preset.getEntityNames();
            }
        }
        
        public Delegator getDelegator() { return delegator; }
        public boolean isUseTrans() { return useTrans; }
        public boolean isNewTrans() { return newTrans; }
        public int getTransTimeout() { return transTimeout; }
        public Collection<String> getTargetEntityNames() { return targetEntityNames; }
        public Collection<String> getEnterEntityNames() { return enterEntityNames; }
        public Collection<String> getEnterMajorEntityNames() { return enterMajorEntityNames; }
        public boolean isIncludeContentRefs() { return includeContentRefs; }
        public boolean isExportFilesAsTextData() { return exportFilesAsTextData; }
        public EntityCondition getEntityCond() { return entityCond; }
        public EntityCondition getEntityDateCond() { return entityDateCond; }
        public Map<String, EntityCondition> getEntityCondMap() { return entityCondMap; }
        public EntityFindOptions getMainEfo() { return mainEfo; }
        public RecordGrouping getRecordGrouping() { return recordGrouping; }
        public boolean isIncludeMajorDeps() { return includeMajorDeps; }
        public int getMaxRecordsPerFile() { return maxRecordsPerFile; }
        public String getAttribTmplAssocType() { return attribTmplAssocType; }
        public String getPmpsMappingTypeId() { return pmpsMappingTypeId; }
        public Boolean getMediaExportVariants() { return mediaExportVariants; }
        
        // NOTE: set* methods ignore null values. for string and collections you can override with empty string and collection, however.
        //public T setDelegator(Delegator delegator) { this.delegator = delegator; return (T) this; } // don't change this
        public T setUseTrans(boolean useTrans) { this.useTrans = useTrans; return (T) this; }
        public T setNewTrans(boolean newTrans) { this.newTrans = newTrans; return (T) this; }
        public T setTransTimeout(int transTimeout) { this.transTimeout = transTimeout; return (T) this; }
        public T setTargetEntityNames(Collection<String> targetEntityNames) { this.targetEntityNames = targetEntityNames; return (T) this; }
        public T setEnterEntityNames(Collection<String> enterEntityNames) { this.enterEntityNames = enterEntityNames; return (T) this; }
        public T setEnterEntityNamesFromPreset(String presetConfigName) { setEnterEntityNamesFromPresetInternal(EntityPresetMap.getInst(delegator).get(presetConfigName)); return (T) this; }
        public T setEnterEntityNamesFromPreset(PresetConfig presetConfig) { setEnterEntityNamesFromPresetInternal(presetConfig); return (T) this; }
        public T setEnterMajorEntityNames(Collection<String> targetMajorEntityNames) { this.enterMajorEntityNames = targetMajorEntityNames; return (T) this; }
        public T setEnterMajorEntityNamesFromPreset(String presetConfigName) { setEnterMajorEntityNamesFromPresetInternal(EntityPresetMap.getInst(delegator).get(presetConfigName)); return (T) this; }
        public T setEnterMajorEntityNamesFromPreset(PresetConfig presetConfig) { setEnterMajorEntityNamesFromPresetInternal(presetConfig); return (T) this; }
        public T setIncludeContentRefs(boolean includeContentRefs) { this.includeContentRefs = includeContentRefs; return (T) this; }
        public T setExportFilesAsTextData(boolean exportFilesAsTextData) { this.exportFilesAsTextData = exportFilesAsTextData; return (T) this; }
        public T setEntityCond(EntityCondition entityCond) { this.entityCond = entityCond; return (T) this; }
        public T setEntityDateCond(EntityCondition entityDateCond) { this.entityDateCond = entityDateCond; return (T) this; }
        public T setEntityCondMap(Map<String, EntityCondition> entityCondMap) { if (entityCondMap != null) this.entityCondMap = entityCondMap; else this.entityCondMap = new HashMap<>(); return (T) this; }
        public T setMainEfo(EntityFindOptions mainEfo) { this.mainEfo = mainEfo; return (T) this; }
        public T setCommonEfo() { this.setMainEfo(CmsDataExportWorker.getCommonEfo()); return (T) this; }
        public T setRecordGrouping(RecordGrouping recordGrouping) { this.recordGrouping = recordGrouping; return (T) this; }
        public T setRecordGrouping(String recordGrouping) { this.recordGrouping = RecordGrouping.fromString(recordGrouping); return (T) this; }
        public T setIncludeMajorDeps(boolean includeMajorDeps) { this.includeMajorDeps = includeMajorDeps; return (T) this; }
        public T setMaxRecordsPerFile(int maxRecordsPerFile) { this.maxRecordsPerFile = maxRecordsPerFile; return (T) this; }
        public T setAttribTmplAssocType(String attribTmplAssocType) { this.attribTmplAssocType = attribTmplAssocType; return (T) this; }
        public T setPmpsMappingTypeId(String pmpsMappingTypeId) { this.pmpsMappingTypeId = pmpsMappingTypeId; return (T) this; } 
        public T setMediaExportVariants(Boolean mediaExportVariants) { this.mediaExportVariants = mediaExportVariants; return (T) this; } 
        
        public T setAllFromMap(Map<String, ?> ctx) { 
            applyPresetFromMap(ctx); 
            setFieldsFromMap(ctx); 
            return (T) this; 
        }
        public T applyPresetFromMap(Map<String, ?> ctx) { 
            if (ctx.containsKey("presetConfigName")) applyPreset((String) ctx.get("presetConfigName"));
            if (ctx.containsKey("enterPresetConfigName")) setEnterEntityNamesFromPreset((String) ctx.get("enterPresetConfigName"));
            if (ctx.containsKey("enterMajorPresetConfigName")) setEnterMajorEntityNamesFromPreset((String) ctx.get("enterMajorPresetConfigName"));
            return (T) this; 
        }
        public T setFieldsFromMap(Map<String, ?> ctx) {
            if (ctx.containsKey("targetEntityNames")) setTargetEntityNames(UtilGenerics.<String>checkCollection(ctx.get("targetEntityNames")));
            if (ctx.containsKey("enterEntityNames")) setEnterEntityNames(UtilGenerics.<String>checkCollection(ctx.get("enterEntityNames")));
            if (ctx.containsKey("enterMajorEntityNames")) setEnterMajorEntityNames(UtilGenerics.<String>checkCollection(ctx.get("enterMajorEntityNames")));
            if (ctx.containsKey("attribTmplAssocType")) setAttribTmplAssocType(getString(ctx, "attribTmplAssocType"));
            if (ctx.containsKey("pmpsMappingTypeId")) setPmpsMappingTypeId(getString(ctx, "pmpsMappingTypeId"));
            
            if (ctx.containsKey("recordGrouping")) setRecordGrouping(getString(ctx, "recordGrouping"));
            if (ctx.containsKey("exportFilesAsTextData")) setExportFilesAsTextData((Boolean) ctx.get("exportFilesAsTextData"));
            if (ctx.containsKey("includeMajorDeps")) setIncludeMajorDeps((Boolean) ctx.get("includeMajorDeps"));
            if (ctx.containsKey("includeContentRefs")) setIncludeContentRefs((Boolean) ctx.get("includeContentRefs"));
            if (ctx.containsKey("maxRecordsPerFile")) setMaxRecordsPerFile((Integer) ctx.get("maxRecordsPerFile"));
            
            if (ctx.containsKey("entityCond")) setEntityCond((EntityCondition) ctx.get("entityCond"));
            if (ctx.containsKey("entityDateCond")) setEntityDateCond((EntityCondition) ctx.get("entityDateCond"));
            if (ctx.containsKey("entityCondMap")) setEntityCondMap(UtilGenerics.<String, EntityCondition>checkMap(ctx.get("entityCondMap")));
            
            if (ctx.containsKey("mainEfo")) setMainEfo((EntityFindOptions) ctx.get("mainEfo"));
            if (Boolean.TRUE.equals(ctx.get("useCommonEfo"))) {
                setCommonEfo();
            }
            if (ctx.containsKey("transTimeout")) setTransTimeout((Integer) ctx.get("transTimeout"));
     
            return (T) this; 
        }
        
        private String getString(Map<String, ?> ctx, String key) { return (String) ctx.get(key); }
        
        // Finalization methods
        /**
         * Returns copy of entityCondMap plus attribTmplAssocType and pmpsMappingTypeId factored in.
         */
        protected Map<String, EntityCondition> getEffectiveEntityCondMap() {
            Map<String, EntityCondition> effCondMap = new HashMap<>(getEntityCondMap());
            if (UtilValidate.isNotEmpty(getPmpsMappingTypeId())) {
                // NOTE: for ObjGrp worker, this gets post-processed in the ObjGrp constructor
                CmsDataExportWorker.addPageSpecialMappingConds(delegator, getPmpsMappingTypeId(), effCondMap);
            }
            if (UtilValidate.isNotEmpty(getAttribTmplAssocType())) {
                CmsDataExportWorker.addAttribTmplAssocTypeConds(delegator, getAttribTmplAssocType(), effCondMap);
            }
            return effCondMap;
        }
        
        protected Set<String> getEffectiveTargetEntityNames() {
            return entityInfo.filterCmsEntityNames((targetEntityNames != null) ? getTargetEntityNames() : Collections.<String>emptySet());
        }
        protected Set<String> getEffectiveSpecialTargetEntityNames() {
            return entityInfo.filterSpecialCmsEntityNames((targetEntityNames != null) ? getTargetEntityNames() : Collections.<String>emptySet());
        }
        protected Set<String> getEffectiveEnterEntityNames() {
            if (enterEntityNames == null || enterEntityNames.isEmpty()) return null;
            return entityInfo.filterCmsEntityNames(getEnterEntityNames());
        }
        protected Set<String> getEffectiveEnterMajorEntityNames() {
            if (enterMajorEntityNames == null || enterMajorEntityNames.isEmpty()) return null;
            return entityInfo.filterMajorCmsEntityNames(getEnterMajorEntityNames());
        }
    }
    
    // NOTE: may have more args subclass later, but simplified to 1.5 for now, as most args could be reused
    public static class GenericWorkerArgs extends CommonWorkerArgs<GenericWorkerArgs> {
        public GenericWorkerArgs(Delegator delegator) { super(delegator); }
    }
    
    protected CmsDataExportWorker(CommonWorkerArgs<?> args) throws IllegalArgumentException {
        this.delegator = args.getDelegator();
        this.useTrans = args.isUseTrans();
        this.newTrans = args.isNewTrans();
        this.transTimeout = args.getTransTimeout();
        this.entityInfo = CmsEntityInfo.getInst(this.delegator);
        this.entityCdataFields = entityInfo.getEntityCdataFields();
        this.exportFilesAsTextData = args.isExportFilesAsTextData();
        this.targetEntityNames = args.getEffectiveTargetEntityNames();
        this.targetSpecialEntityNames = args.getEffectiveSpecialTargetEntityNames();
        Set<String> targetCombinedEntityNames = new LinkedHashSet<>();
        targetCombinedEntityNames.addAll(this.targetEntityNames);
        targetCombinedEntityNames.addAll(this.targetSpecialEntityNames);
        this.targetCombinedEntityNames = Collections.unmodifiableSet(targetCombinedEntityNames);
        this.entityCond = args.getEntityCond();
        this.entityDateCond = args.getEntityDateCond();
        this.entityCondMap = Collections.unmodifiableMap(args.getEffectiveEntityCondMap());
        this.includeContentRefs = args.isIncludeContentRefs();
        this.mainEfo = args.getMainEfo();
        this.attribTmplAssocType = args.getAttribTmplAssocType();
        this.pmpsMappingTypeId = args.getPmpsMappingTypeId();
        this.doSpecialProcessViewMappingFilter = (args.getRecordGrouping() != RecordGrouping.MAJOR_OBJECT) && entityCondMap.get("CmsProcessMapping") != null;
        if (CmsUtil.verboseOn()) {
            Debug.logInfo(LOG_PREFIX+"Using special CmsProcessViewMapping filter? " + this.doSpecialProcessViewMappingFilter, module);
        }
        Boolean mediaExportVariants = args.getMediaExportVariants();
        if (mediaExportVariants == null) mediaExportVariants = getTargetSpecialEntityNames().contains("CmsMediaVariants");
        this.mediaExportVariants = mediaExportVariants;
    }
    
    protected CmsDataExportWorker(CmsDataExportWorker other, Delegator delegator) {
        this.delegator = (delegator != null) ? delegator : other.delegator;
        this.useTrans = other.useTrans;
        this.newTrans = other.newTrans;
        this.transTimeout = other.transTimeout;
        this.entityInfo = CmsEntityInfo.getInst(this.delegator);
        this.entityCdataFields = this.entityInfo.getEntityCdataFields();
        this.exportFilesAsTextData = other.exportFilesAsTextData;
        this.targetEntityNames = other.targetEntityNames;
        this.targetSpecialEntityNames = other.targetSpecialEntityNames;
        this.targetCombinedEntityNames = other.targetCombinedEntityNames;
        this.entityCond = other.entityCond;
        this.entityDateCond = other.entityDateCond;
        this.entityCondMap = other.entityCondMap;
        this.includeContentRefs = other.includeContentRefs;
        this.mainEfo = other.mainEfo;
        this.attribTmplAssocType = other.attribTmplAssocType;
        this.pmpsMappingTypeId = other.pmpsMappingTypeId;
        this.doSpecialProcessViewMappingFilter = other.doSpecialProcessViewMappingFilter;
        this.mediaExportVariants = other.mediaExportVariants;
    }
    
    /**
     * Clones the worker but with a fresh new state, with optional replacement delegator (pass null to keep).
     */
    public abstract CmsDataExportWorker cloneWorkerNewState(Delegator delegator);

    public static CmsDataExportWorker makeGenericWorker(GenericWorkerArgs args) throws IllegalArgumentException {
        return new GenericWorker(args);
    }

    public static SingleFileWorker makeSingleFileWorker(GenericWorkerArgs args) throws IllegalArgumentException {
        if (args.getRecordGrouping() == RecordGrouping.MAJOR_OBJECT) return new ObjGrpSingleFileWorker(args);
        else return new SingleFileWorker(args);
    }

    public static MultiFileWorker makeMultiFileWorker(GenericWorkerArgs args) throws IllegalArgumentException {
        return new MultiFileWorker(args);
    }
    
    /******************************************************/
    /* Getters/Setters/Info */
    /******************************************************/

    public Delegator getDelegator() {
        return delegator;
    }
    
    /**
     * Gets the target CMS entity names (i.e. gotten via passedEntityNames request parameter).
     * NOTE: These may be reordered compared to the ones passed to the controller.
     */
    public Set<String> getTargetEntityNames() {
        return targetEntityNames;
    }
    
    /**
     * Gets the SPECIAL target CMS entity names (i.e. gotten via passedEntityNames request parameter), can include fake entities,
     * e.g. "CmsMedia".
     * NOTE: These may be reordered compared to the ones passed to the controller.
     */
    public Set<String> getTargetSpecialEntityNames() {
        return targetSpecialEntityNames;
    }
    
    /**
     * Returns {@link #getTargetEntityNames} + {@link #getTargetSpecialEntityNames}.
     */
    public Set<String> getTargetCombinedEntityNames() {
        return targetCombinedEntityNames;
    }
    
    /**
     * Sanity check, don't use in implementation.
     */
    public boolean hasEntityNames() {
        return targetCombinedEntityNames.size() > 0;
    }

    public EntityCondition getEntityCond() {
        return entityCond;
    }
    
    public EntityCondition getEntityDateCond() {
        return entityDateCond;
    }
    
    public Map<String, EntityCondition> getEntityCondMap() {
        return Collections.unmodifiableMap(entityCondMap);
    }
    
    protected Map<String, EntityCondition> getEntityCondMapInternal() {
        return entityCondMap;
    }

    public boolean isExportFilesAsTextData() {
        return exportFilesAsTextData;
    }

    public Set<String> getSeenContentIds() {
        return seenContentIds;
    }

    public abstract boolean isMultiFile();
    
    public boolean isSingleFile() {
        return !isMultiFile();
    }

    public PrintWriter getWriter() {
        return writer;
    }

    public void setWriter(PrintWriter writer) {
        this.writer = writer;
    }
    
    public EntityFindOptions getMainEfo() {
        return mainEfo;
    }

    public void setMainEfo(EntityFindOptions mainEfo) {
        this.mainEfo = mainEfo;
    }

    public ModelEntity getCurrentRecordModel() {
        return currentRecordModel;
    }
    
    /**
     * WARN: this may be different from getCurrentRecordModel().getEntityName()!
     */
    public String getCurrentRecordName() {
        return currentRecordName;
    }

    public void setCurrentRecordModel(ModelEntity currentRecordModel) {
        this.currentRecordModel = currentRecordModel;
        this.setCurrentContentIdFieldNames(entityInfo.getCmsContentIdFieldNames(currentRecordModel));
    }
    
    public boolean isIncludeContentRefs() {
        return includeContentRefs;
    }
    
    public Set<String> getCurrentContentIdFieldNames() {
        return Collections.unmodifiableSet(currentContentIdFieldNames);
    }

    protected void setCurrentContentIdFieldNames(Set<String> currentContentIdFieldNames) {
        this.currentContentIdFieldNames = currentContentIdFieldNames;
    }

    public boolean hasSeenContentId(String contentId) {
        return seenContentIds.contains(contentId);
    }
    
    protected boolean registerContentId(String contentId) {
        return seenContentIds.add(contentId);
    }
    
    protected abstract RecordGrouping getRecordGrouping();


    public EntityCondition getEntitySpecificCond(String entityName) {
        return entityCondMap.get(entityName);
    }
    
    public EntityCondition getEffectiveEntityCond(String entityName, boolean excludeDateCond) {
        List<EntityCondition> condList = new ArrayList<>(4);
        EntityCondition commonCond = getEntityCond();
        if (commonCond != null) {
            condList.add(commonCond);
        }
        if (!excludeDateCond) {
            EntityCondition dateCond = getEntityDateCond();
            if (dateCond != null) {
                condList.add(dateCond);
            }
        }
        EntityCondition specCond = getEntitySpecificCond(entityName);
        if (specCond != null) {
            condList.add(specCond);
        }
        // SPECIAL: CmsProcessViewMapping workaround
        // FIXME: this is a memory hog solution generating a huge query but it's the only one on the table for now
        if (doSpecialProcessViewMappingFilter && "CmsProcessViewMapping".equals(entityName) && this.seenCmsProcessMappingIds.size() > 0) {
            List<EntityCondition> viewCondList = new ArrayList<>(this.seenCmsProcessMappingIds.size());
            for(String id : this.seenCmsProcessMappingIds) {
                viewCondList.add(EntityCondition.makeCondition("processMappingId", id));
            }
            condList.add(EntityCondition.makeCondition(viewCondList, EntityOperator.OR));
        }
        // not for now
        if ("CmsMedia".equals(this.getCurrentRecordName())) {
            condList.add(EntityCondition.makeCondition("contentTypeId", "SCP_MEDIA"));
        }
        return condList.isEmpty() ? null : EntityCondition.makeCondition(condList, EntityOperator.AND);
    }
    
    public EntityCondition getEffectiveEntityCond(String entityName) {
        return getEffectiveEntityCond(entityName, false);
    }
    
    public boolean isMediaExportVariants() {
        return mediaExportVariants;
    }

    
    /******************************************************/
    /* Essential/Configuration Variables */
    /******************************************************/

    /**
     * How to group records.
     * NOTE: this is currently only used for single-file exports.
     */
    public enum RecordGrouping {
        /**
         * No grouping, fastest export.
         */
        NONE("CmsRecordGroupingNoGrouping", "CmsRecordGroupingNoGroupingHint"),
        /**
         * Grouping by entity type (name), fastest import, but has export performance issues.
         * NOTE: the export performance is worst for single-file (memory issues), whereas multi-file may
         * manage better.
         */
        ENTITY_TYPE("CmsRecordGroupingGroupByEntityType", "CmsRecordGroupingGroupByEntityTypeHint"),
        /**
         * Grouping by object, slowest export and import, but most readable.
         */
        MAJOR_OBJECT("CmsRecordGroupingGroupByMajorObject", "CmsRecordGroupingGroupByMajorObjectHint");
        
        public static final RecordGrouping DEFAULT = getDisplayValues().get(0); // default = always the first
        public static final String LABEL_RESOURCE = "CMSUiLabels";
        //private static final List<RecordGrouping> displayValues = UtilMisc.unmodifiableArrayList(NONE, ENTITY_TYPE);
        
        private final String labelName;
        private final String hintLabelName;
        
        private RecordGrouping(String labelName, String hintLabelName) {
            this.labelName = labelName;
            this.hintLabelName = hintLabelName;
        }
        
        public String getLabelResource() { return LABEL_RESOURCE; }
        public String getLabelName() { return labelName; }
        public String getLabel(Locale locale) { return UtilProperties.getMessage(getLabelResource(), getLabelName(), locale); }
        public String getHintLabelName() { return hintLabelName; }
        public String getHintLabel(Locale locale) { return UtilProperties.getMessage(getLabelResource(), getHintLabelName(), locale); }
        public String getFullLabel(Locale locale) { return getLabel(locale) + " (" + getHintLabel(locale) + ")"; }
        public static RecordGrouping fromString(String str) { return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : null; }
        public static RecordGrouping fromStringSafe(String str) {
            try {
                return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : null;
            } catch(Exception e) { return null; }
        }
        public static RecordGrouping fromStringOrDefault(String str) { return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : DEFAULT; }
        public static RecordGrouping fromStringOrDefaultSafe(String str) {
            try {
                return UtilValidate.isNotEmpty(str) ? RecordGrouping.valueOf(str) : DEFAULT;
            } catch(Exception e) { return DEFAULT; }
        }
        public static RecordGrouping getDefault() { return DEFAULT; }
        public static List<RecordGrouping> getDisplayValues() { return Arrays.asList(RecordGrouping.values()); } // return displayValues; }
    }
    
    public enum OutputMode {
        SF_DL("CmsSingleFileDownload", null),
        SF_IL("CmsSingleFileInline", "CmsSingleFileInlineDesc"),
        SF_FS("CmsSingleFileServerFile", null),
        MF_FS("CmsMultiFileServerDirectory", "CmsExportFuncMultiFileInfo");
        
        public static final String LABEL_RESOURCE = "CMSUiLabels";
        private static final List<OutputMode> restrictedDisplayModes = Collections.unmodifiableList(new ArrayList<>(Arrays.asList(OutputMode.values())));
        private static final List<OutputMode> allowedAllDisplayModes = UtilMisc.unmodifiableArrayList(SF_DL, SF_IL);
        public static final OutputMode DEFAULT = getDisplayValues().get(0); // default = always the first
        
        private final String labelName;
        private final String descLabelName;

        private OutputMode(String labelName, String descLabelName) { this.labelName = labelName; this.descLabelName = descLabelName; }

        public boolean isSingle() { return this == SF_IL || this == SF_DL || this == SF_FS; }
        public boolean isMulti() { return this == MF_FS; }
        public String getLabelResource() { return LABEL_RESOURCE; }
        public String getLabelName() { return labelName; }
        public String getLabel(Locale locale) { return UtilProperties.getMessage(getLabelResource(), getLabelName(), locale); }
        public String getDescLabelName() { return descLabelName; }
        public String getDescLabel(Locale locale) { return descLabelName != null ? UtilProperties.getMessage(getLabelResource(), getDescLabelName(), locale) : null; }
        public static OutputMode fromString(String str) { return UtilValidate.isNotEmpty(str) ? OutputMode.valueOf(str) : null; }
        public static OutputMode fromStringSafe(String str) {
            try {
                return UtilValidate.isNotEmpty(str) ? OutputMode.valueOf(str) : null;
            } catch(Exception e) { return null; }
        }
        public static OutputMode fromStringOrDefault(String str) { return UtilValidate.isNotEmpty(str) ? OutputMode.valueOf(str) : DEFAULT; }
        public static OutputMode fromStringOrDefaultSafe(String str) {
            try {
                return UtilValidate.isNotEmpty(str) ? OutputMode.valueOf(str) : DEFAULT;
            } catch(Exception e) { return DEFAULT; }
        }
        public static OutputMode getDefault() { return DEFAULT; }
        public static List<OutputMode> getDisplayValues() { return allowedAllDisplayModes; }
        public static List<OutputMode> getDisplayValues(Security security, GenericValue userLogin) { 
            if (security != null && userLogin != null && security.hasPermission("ENTITY_MAINT", userLogin)) {
                return restrictedDisplayModes;
            } else {
                return allowedAllDisplayModes; 
            }
        }
        public void checkAllowed(Security security, GenericValue userLogin) throws IllegalStateException {
            if (allowedAllDisplayModes.contains(this)) return;
            else {
                if (!security.hasPermission("ENTITY_MAINT", userLogin)) {
                    throw new IllegalStateException("User missing ENTITY_MAINT permission for output mode " + this); // FIXME: more appropriate exception
                } 
            }
        }
    }
    
    /**
     * Presets for entity selections and queries.
     * See {@link #buildDefaultEntityPresets} body for available presets, or simply visit Cms Data Export page.
     */
    public static class EntityPresetMap implements Serializable, Map<String, PresetConfig> {
        public static final String module = EntityPresetMap.class.getName();
        private static final EntityPresetMap INSTANCE = buildDefaultEntityPresets(DelegatorFactory.getDelegator("default")); // optimization: only need one inst
        protected final Map<String, PresetConfig> map;
        protected final Set<String> simpleOptionKeys;

        /**
         * Main constructor, private - use {@link #buildDefaultEntityPresets} or {@link PresetConfig.Builder}.
         */
        EntityPresetMap(Map<String, PresetConfig> map, Set<String> simpleOptionKeys) { 
            this.map = map; 
            this.simpleOptionKeys = (simpleOptionKeys != null) ? Collections.unmodifiableSet(simpleOptionKeys) : null;
        }
        
        /**
         * Returns a map of export presets to the names of the entities they should export.
         */
        public static EntityPresetMap getInst(Delegator delegator) {
            return INSTANCE;
        }
        
        /**
         * Builds map of export preset names to the names of the entities they should export and other settings;
         * this overload adds to given builder.
         */
        public static PresetConfig.Builder buildDefaultEntityPresets(PresetConfig.Builder builder) {
            PresetConfig.Builder b = builder;
            String pfx = CmsEntityInfo.CMS_ENTITY_BASE_PKG_PREFIX;
            CmsEntityInfo ei = b.getCmsEntityInfo();
            
            // NOTE: the variants take up huge space, so we exclude them by default
            b.newConfig("CmsAllEntities").setEntityNames(ei.copyCmsEntityNames(null, ei.getCombinedCmsEntityNames(), b.list("CmsMediaVariants"))).complete();
            b.newConfig("CmsAllEntitiesNoMedia").setEntityNames(ei.getCmsEntityNames()).complete();
            
            // NOTE: CmsPages includes the CmsProcessMapping and CmsPageSpecialMapping in order to save the primary path
            b.newConfig("CmsPages").setPmpsMappingTypeId("CMS_PGSPCMAP_PRIMARY").setEntityNames(ei.copyCmsEntityNames(b.list(pfx+"content"), b.list("CmsProcessMapping", "CmsProcessViewMapping", "CmsPageSpecialMapping"))).complete();
            b.newConfig("CmsPagesStrict").setEntityNames(ei.copyCmsEntityNames(b.list(pfx+"content"))).complete();
            
            // NOTE: CmsMappings tries to exclude primary process mappings 
            b.newConfig("CmsMappings").setPmpsMappingTypeId("CMS_PGSPCMAP_STD").setEntityNames(ei.copyCmsEntityNames(pfx+"control")).complete();
            b.newConfig("CmsMappingsStrict").setEntityNames(ei.copyCmsEntityNames(pfx+"control")).complete();
            b.newConfig("CmsPagesMappings").setEntityNames(ei.copyCmsEntityNames(b.list(pfx+"content", pfx+"control"))).complete();
            
            b.newConfig("CmsTemplates").setEntityNames(ei.copyCmsEntityNames(pfx+"template")).complete();
            b.newConfig("CmsPageTemplates").setAttribTmplAssocType("PAGE_TEMPLATE").setEntityNames(ei.copyCmsEntityNames(null, b.list("CmsPageTemplate", "CmsPageTemplateAssetAssoc", "CmsPageTemplateScriptAssoc", "CmsPageTemplateVersion", "CmsPageTemplateVersionState", "CmsAttributeTemplate"))).complete();
            b.newConfig("CmsAssetTemplates").setAttribTmplAssocType("ASSET_TEMPLATE").setEntityNames(ei.copyCmsEntityNames(null, b.list("CmsAssetTemplate", "CmsAssetTemplateScriptAssoc", "CmsAssetTemplateVersion", "CmsAssetTemplateVersionState", "CmsAttributeTemplate"))).complete();
            b.newConfig("CmsScriptTemplates").setEntityNames(ei.copyCmsEntityNames(null, b.list("CmsScriptTemplate"))).complete();
            
            // SPECIAL CASES
            b.newConfig("CmsMedia").setEntityNames(UtilMisc.toHashSet("CmsMedia")).complete();
            b.newConfig("CmsMediaWithVariants").setEntityNames(UtilMisc.toHashSet("CmsMedia", "CmsMediaVariants")).complete();
            
            // Simplified (non-advanced) list of names to show in the UI...
            // this must be separate because internally we need access to all the presets
            b.setSimplePresetNames(new LinkedHashSet<>(Arrays.asList(new String[] {
                    "CmsAllEntities", "CmsAllEntitiesNoMedia", "CmsPages", "CmsPagesMappings", "CmsTemplates", "CmsPageTemplates", "CmsAssetTemplates", "CmsScriptTemplates",
                    // SPECIAL CASES
                    "CmsMedia", "CmsMediaWithVariants"
            })));

            return b;
        }
        
        /**
         * Builds map of export preset names to the names of the entities they should export and other settings;
         * this overload creates a builder with a LinkedHashMap and unmodifiable EntityPresetMap.
         */
        public static EntityPresetMap buildDefaultEntityPresets(Delegator delegator) {
            return buildDefaultEntityPresets(new PresetConfig.Builder(delegator, new LinkedHashMap<String, PresetConfig>())).completePresetMap();
        }

        @Override public int size() { return map.size(); }
        @Override public boolean isEmpty() { return map.isEmpty(); }
        @Override public boolean containsKey(Object key) { return map.containsKey(key); }
        @Override public boolean containsValue(Object value) { return map.containsValue(value); }
        @Override public PresetConfig get(Object key) { return map.get(key); }
        @Override public PresetConfig put(String key, PresetConfig value) { return map.put(key, value); }
        @Override public PresetConfig remove(Object key) { return map.remove(key); }
        @Override public void putAll(Map<? extends String, ? extends PresetConfig> m) { map.putAll(m); }
        @Override public void clear() { map.clear(); }
        @Override public Set<String> keySet() { return map.keySet(); }
        @Override public Collection<PresetConfig> values() { return map.values(); }
        @Override public Set<Map.Entry<String, PresetConfig>> entrySet() { return map.entrySet(); }
        @Override public boolean equals(Object o) { return map.equals(o); }
        
        public PresetConfig getOrNone(Object key) { PresetConfig res = get(key); return res != null ? res : PresetConfig.NONE; }
        public PresetConfig getOrDefaults(Object key) { PresetConfig res = get(key); return res != null ? res : PresetConfig.DEFAULTS; }
        
        public Collection<String> getAllPresetNames() { return keySet(); }
        public Collection<String> getSimplePresetNames() { return (simpleOptionKeys != null) ? simpleOptionKeys : keySet(); }
    }

    /**
     * Preset export config. Immutable and automatically doubles as a map itself.
     */
    public static class PresetConfig implements Serializable, Map<String, Object> {
        public static final PresetConfig NONE;
        public static final PresetConfig DEFAULTS;
        static {
            PresetConfig.Builder b = new PresetConfig.Builder(DelegatorFactory.getDelegator("default"), null);
            NONE = b.newConfig("None").complete();
            DEFAULTS = b.newConfig("Defaults").complete();
        }
        
        protected final String presetName;
        protected final String pmpsMappingTypeId;
        protected final String attribTmplAssocType;
        protected final String labelName;
        protected final Set<String> entityNames;
        
        protected final Map<String, Object> map;

        private PresetConfig(Builder other) {
            this.presetName = other.presetName;
            this.pmpsMappingTypeId = other.pmpsMappingTypeId;
            this.attribTmplAssocType = other.attribTmplAssocType;
            this.labelName = (other.labelName != null) ? other.labelName : ("CmsEntityPresetLabel_" + this.presetName);
            this.entityNames = (other.entityNames != null) ? Collections.unmodifiableSet(other.entityNames) : null;
            this.map = Collections.unmodifiableMap(toMap(new HashMap<String, Object>()));
        }

        public String getPresetName() { return presetName; }
        public String getPmpsMappingTypeId() { return pmpsMappingTypeId; }
        public String getAttribTmplAssocType() { return attribTmplAssocType; }
        public String getLabelName() { return labelName; }
        public Set<String> getEntityNames() { return entityNames; }
        
        private Map<String, Object> toMap(Map<String, Object> other) {
            other.put("presetName", presetName);
            other.put("pmpsMappingTypeId", pmpsMappingTypeId);
            other.put("attribTmplAssocType", attribTmplAssocType);
            other.put("labelName", labelName);
            other.put("entityNames", entityNames);
            return other;
        }
        
        /**
         * Dual PresetConfig and EntityPresetMap builder.
         */
        public static class Builder {
            private Delegator delegator;
            private CmsEntityInfo ei;
            private Map<String, PresetConfig> rawEntityPresetMap;
            
            protected String presetName;
            protected String pmpsMappingTypeId;
            protected String attribTmplAssocType;
            protected String labelName;
            protected Set<String> entityNames;
            protected Set<String> simpleOptionKeys = null;
            
            /**
             * Builder with a LinkedHashMap for rawEntityPresetMap.
             */
            public Builder(Delegator delegator) { this(delegator, new LinkedHashMap<String, PresetConfig>()); }
            /**
             * Builder with a custom map or no map (to create only configs).
             */
            public Builder(Delegator delegator, Map<String, PresetConfig> rawEntityPresetMap) { 
                this.delegator = delegator; 
                this.rawEntityPresetMap = rawEntityPresetMap; 
                this.ei = CmsEntityInfo.getInst(delegator);
            }
            public Builder newConfig(String presetName) { resetPreset(); setPresetName(presetName); return this; }
            public Builder resetPreset() {
                this.presetName = null;
                this.pmpsMappingTypeId = null;
                this.attribTmplAssocType = null;
                this.labelName = null;
                this.entityNames = null;
                return this;
            }
            public Builder setAll(PresetConfig other) { // setAll
                this.presetName = other.presetName;
                this.pmpsMappingTypeId = other.pmpsMappingTypeId;
                this.attribTmplAssocType = other.attribTmplAssocType;
                this.labelName = other.labelName;
                this.entityNames = other.entityNames;
                return this;
            }
            
            public Delegator getDelegator() { return delegator; } 
            public CmsEntityInfo getCmsEntityInfo() { return ei; } 
            
            public String getPresetName() { return presetName; }
            public Builder setPresetName(String presetName) { this.presetName = presetName; return this; }
            public Builder setPmpsMappingTypeId(String pmpsMappingTypeId) { this.pmpsMappingTypeId = pmpsMappingTypeId; return this; }
            public Builder setAttribTmplAssocType(String attribTmplAssocType) { this.attribTmplAssocType = attribTmplAssocType; return this; }
            public Builder setLabelName(String labelName) { this.labelName = labelName; return this; }
            public Builder setEntityNames(Set<String> entityNames) { this.entityNames = entityNames; return this; }
            public void setSimplePresetNames(Set<String> simpleOptionKeys) { this.simpleOptionKeys = simpleOptionKeys; }
            
            @SafeVarargs public final <T> List<T> list(T... args) { return new ArrayList<>(Arrays.asList(args)); }            
            @SafeVarargs public final <T> Set<T> set(T... args) { return new LinkedHashSet<>(Arrays.asList(args)); }  
            
            /**
             * Builds the PresetConfig and adds it to the internal map.
             */
            public PresetConfig complete() { 
                PresetConfig pc = new PresetConfig(this); 
                if (rawEntityPresetMap != null) rawEntityPresetMap.put(pc.getPresetName(), pc);
                return pc;
            }
            /**
             * Returns a finalized, immutable {@link EntityPresetMap}.
             */
            public EntityPresetMap completePresetMap() { return new EntityPresetMap(Collections.unmodifiableMap(rawEntityPresetMap), simpleOptionKeys); }
        }

        @Override public int size() { return map.size(); }        
        @Override public boolean isEmpty() { return map.isEmpty(); }
        @Override public boolean containsKey(Object key) { return map.containsKey(key); }
        @Override public boolean containsValue(Object value) { return map.containsValue(value); }
        @Override public Object get(Object key) { return map.get(key); }
        @Override public Object put(String key, Object value) { return map.put(key, value); }
        @Override public Object remove(Object key) { return map.remove(key); }
        @Override public void putAll(Map<? extends String, ? extends Object> m) { map.putAll(m); }
        @Override public void clear() { map.clear(); }
        @Override public Set<String> keySet() { return map.keySet(); }
        @Override public Collection<Object> values() { return map.values(); }
        @Override public Set<Map.Entry<String, Object>> entrySet() { return map.entrySet(); }
        @Override public boolean equals(Object o) { return map.equals(o); }
    }
    

    /******************************************************/
    /* Entity Query helpers */
    /******************************************************/
    
    // FIXME: this is duplicated in the CmsEntityVisit class; both are in use
    /**
     * Gets content and related values.
     * <p>
     * FIXME: this is duplicated in {@link com.ilscipio.scipio.cms.data.CmsEntityVisit#acceptContentRelationEntityDepsVisitor}; both are in use.
     */
    public void getContentAndRelatedValues(String contentId, List<GenericValue> values) throws GenericEntityException {
        GenericValue content = delegator.findOne("Content", UtilMisc.toMap("contentId", contentId), false);
        if (content != null) {
            GenericValue dataRes = content.getRelatedOne("DataResource", false);
            if (dataRes != null) {
                if ("ELECTRONIC_TEXT".equals(dataRes.getString("dataResourceTypeId"))) {
                    values.add(dataRes);
                    GenericValue elecText = dataRes.getRelatedOne("ElectronicText", false);
                    if (elecText != null) {
                        values.add(elecText);
                    } else {
                        Debug.logError(LOG_PREFIX+"Missing ElectronicText for contentId '" + contentId + "'", module);
                    }
                } else if (exportFilesAsTextData) {
                    try {
                        TemplateBodySource tmplBodySrc = com.ilscipio.scipio.cms.template.CmsTemplate.getTemplateBodySourceFromContent(delegator, contentId, false);
                        
                        dataRes.put("dataResourceTypeId", "ELECTRONIC_TEXT"); // WARN: DO NOT COMMIT THIS VALUE!
                        values.add(dataRes);
                        
                        GenericValue elecText = delegator.makeValue("ElectronicText", 
                            UtilMisc.toMap("dataResourceId",dataRes.getString("dataResourceId"), "textData",tmplBodySrc.getEffectiveBody()));
                        values.add(elecText);
                    } catch(Exception e) {
                        Debug.logError(e, LOG_PREFIX+"Unable to read content body for contentId '" + contentId + "'", module);
                    }
                } else {
                    values.add(dataRes);
                }
            } else {
                Debug.logError(LOG_PREFIX+"Missing DataResource for contentId '" + contentId + "'", module);
            }
            // TODO?: in the future there may be more related records (ALTERNATE LOCALES); not going too deep yet
            values.add(content);
        }
    }
    
    public List<GenericValue> getContentAndRelatedValues(String contentId) throws GenericEntityException {
        List<GenericValue> values = new ArrayList<>();
        getContentAndRelatedValues(contentId, values);
        return values;
    }
    
    public Map<String, List<GenericValue>> getContentAndRelatedValuesByEntity(Set<String> contentIds) throws GenericEntityException {
        Map<String, List<GenericValue>> map = new LinkedHashMap<>();
        // establish order
        for(String name : extEntityOrder) {
            map.put(name, new ArrayList<GenericValue>());
        }
        for(String contentId : contentIds) {
            List<GenericValue> values = getContentAndRelatedValues(contentId);
            for(GenericValue value : values) {
                String entityName = value.getEntityName();
                List<GenericValue> entityValues = map.get(entityName);
                if (entityValues == null) {
                    entityValues = new ArrayList<>();
                    map.put(entityName, entityValues);
                }
                entityValues.add(value);
            }
        }
        return map;
    }
    
    public static void addToEntityCondMap(Delegator delegator, Map<String, EntityCondition> entityCondMap, String entityName, EntityCondition cond) {
        EntityCondition prevCond = entityCondMap.get(entityName);
        if (prevCond == null) {
            entityCondMap.put(entityName, cond);
        } else {
            entityCondMap.put(entityName, EntityCondition.makeCondition(prevCond, EntityOperator.AND, cond));
        }
    }
    
    public static EntityCondition makeEntityDateCond(Delegator delegator, Timestamp entityFrom, Timestamp entityThru) {
        EntityCondition entityFromCond = null;
        EntityCondition entityThruCond = null;
        EntityCondition entityDateCond = null;
        if (entityFrom != null) {
            entityFromCond = EntityCondition.makeCondition("lastUpdatedTxStamp", EntityComparisonOperator.GREATER_THAN, entityFrom);
        }
        if (entityThru != null) {
            entityThruCond = EntityCondition.makeCondition("lastUpdatedTxStamp", EntityComparisonOperator.LESS_THAN, entityThru);
        }
        if (entityFromCond != null && entityThruCond != null) {
            entityDateCond = EntityCondition.makeCondition(entityFromCond, EntityJoinOperator.AND, entityThruCond);
        } else if (entityFromCond != null) {
            entityDateCond = entityFromCond;
        } else if (entityThruCond != null) {
            entityDateCond = entityThruCond;
        }
        return entityDateCond;
    }
    
    public static EntityCondition makeEntityDateCond(Delegator delegator, String entityFromStr, String entityThruStr) {
        Timestamp entityFrom = UtilValidate.isNotEmpty(entityFromStr) ? UtilDateTime.toTimestamp(entityFromStr) : null;
        Timestamp entityThru = UtilValidate.isNotEmpty(entityThruStr) ? UtilDateTime.toTimestamp(entityThruStr) : null;
        return makeEntityDateCond(delegator, entityFrom, entityThru);
    }
    
    public static GenericValue getStdPageSpecialMappingEnumValue(Delegator delegator) throws GenericEntityException {
        // TODO: review
        // this record shouldn't exist in the system, doesn't make sense, so we make a virtual one for UI use only
        GenericValue value = delegator.findOne("Enumeration", UtilMisc.toMap("enumId", "CMS_PGSPCMAP_STD"), true);
        if (value == null) {
            value = delegator.makeValue("Enumeration", UtilMisc.toMap(
                    "enumId", "CMS_PGSPCMAP_STD",
                    "enumCode", "STD",
                    "sequenceId", "01",
                    "enumTypeId", "CMS_PAGE_SPCMAP_TYPE",
                    "description", "Standard"));
            value.setImmutable(); // don't persist it...
        }
        return value;
    }

    public static List<GenericValue> getPageSpecialMappingCondEnumerations(Delegator delegator) throws GenericEntityException {
        List<GenericValue> realValues = delegator.findByAnd("Enumeration", 
                UtilMisc.toMap("enumTypeId", "CMS_PAGE_SPCMAP_TYPE"), UtilMisc.toList("sequenceId"), true);
        
        List<GenericValue> results = new ArrayList<>();
        // TODO/FIXME: we can only do STD & PRIMARY for now, remove this filter later
        boolean hasStd = false;
        for(GenericValue value : realValues) {
            if ("CMS_PGSPCMAP_STD".equals(value.getString("enumId"))) {
                hasStd = true;
                results.add(value);
            } else if ("CMS_PGSPCMAP_PRIMARY".equals(value.getString("enumId"))) {
                results.add(value);
            }
        }
        if (!hasStd) {
            results.add(0, getStdPageSpecialMappingEnumValue(delegator));
        }
        return results;
    }
    
    public static List<GenericValue> getPageSpecialMappingCondEnumerationsSafe(Delegator delegator) {
        try {
            return getPageSpecialMappingCondEnumerations(delegator);
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return Collections.emptyList();
        }
    }
    
    public static void addPageSpecialMappingConds(Delegator delegator, String pmpsMappingTypeId, Map<String, EntityCondition> entityCondMap) {
        if (UtilValidate.isEmpty(pmpsMappingTypeId)) return;
        
        // FIXME: these conditions are bad, because we can't do complex queries, but close enough for now
        if ("CMS_PGSPCMAP_STD".equals(pmpsMappingTypeId)) { // this is sort of like "NOT primary", but not exact 
            // special case
            addToEntityCondMap(delegator, entityCondMap, "CmsProcessMapping", EntityCondition.makeCondition("primaryForPageId", EntityOperator.EQUALS, null));
            addToEntityCondMap(delegator, entityCondMap, "CmsPageSpecialMapping", EntityCondition.makeCondition("mappingTypeId", EntityOperator.NOT_EQUAL, "CMS_PGSPCMAP_PRIMARY"));
            // FIXME: MISSING CmsProcessViewMapping condition to link to CmsProcessMapping!
        } else if ("CMS_PGSPCMAP_PRIMARY".equals(pmpsMappingTypeId)) {
            addToEntityCondMap(delegator, entityCondMap, "CmsProcessMapping", EntityCondition.makeCondition("primaryForPageId", EntityOperator.NOT_EQUAL, null));
            addToEntityCondMap(delegator, entityCondMap, "CmsPageSpecialMapping", EntityCondition.makeCondition("mappingTypeId", EntityOperator.EQUALS, "CMS_PGSPCMAP_PRIMARY"));
            // FIXME: MISSING CmsProcessViewMapping condition to link to CmsProcessMapping!
        } else {
            // TODO: others would be harder, require an sql "IN"
            throw new UnsupportedOperationException("Process mapping page special mapping filter doesn't yet support: " + pmpsMappingTypeId);
        } 
    }

    public static void addAttribTmplAssocTypeConds(Delegator delegator, String attribTmplAssocType, Map<String, EntityCondition> entityCondMap) {
        if (UtilValidate.isEmpty(attribTmplAssocType)) return;
        if ("PAGE_TEMPLATE".equals(attribTmplAssocType)) {
            addToEntityCondMap(delegator, entityCondMap, "CmsAttributeTemplate", 
                    EntityCondition.makeCondition("pageTemplateId", EntityOperator.NOT_EQUAL, null));
        } else if ("ASSET_TEMPLATE".equals(attribTmplAssocType)) {
            addToEntityCondMap(delegator, entityCondMap, "CmsAttributeTemplate", 
                    EntityCondition.makeCondition("assetTemplateId", EntityOperator.NOT_EQUAL, null));
        } else {
            throw new UnsupportedOperationException("Unrecognized attribTmplAssocType value: " + attribTmplAssocType);
        } 
    }
    
    public static void addEntityPkFilterConds(Delegator delegator, Map<String, ?> entityPks, Map<String, EntityCondition> entityCondMap) {
        for(Map.Entry<String, ?> entry : entityPks.entrySet()) {
            String entityName = entry.getKey();
            Object pkVals = entry.getValue();
            if (pkVals instanceof String) {
                addEntityPkFilterConds(delegator, entityName, (String) pkVals, entityCondMap);
            } else if (pkVals instanceof Collection) {
                addEntityPkFilterConds(delegator, entityName, UtilGenerics.<String>checkCollection(pkVals), entityCondMap);
            }
        }
        
    }
    
    public static void addEntityPkFilterConds(Delegator delegator, String entityName, String pkValue, Map<String, EntityCondition> entityCondMap) throws IllegalArgumentException {
        EntityCondition cond = EntityCondition.makeCondition(EntityInfoUtil.getSinglePkFieldNameStrict(delegator, entityName), pkValue);
        addToEntityCondMap(delegator, entityCondMap, entityName, cond);
    }
    
    public static void addEntityPkFilterConds(Delegator delegator, String entityName, Collection<String> pkValues, Map<String, EntityCondition> entityCondMap) throws IllegalArgumentException {
        String pkFieldName = EntityInfoUtil.getSinglePkFieldNameStrict(delegator, entityName);
        List<EntityCondition> condList = new ArrayList<>(pkValues.size());
        for(String pkValue : pkValues) {
            condList.add(EntityCondition.makeCondition(pkFieldName, pkValue));
        }
        EntityCondition cond = EntityCondition.makeCondition(condList, EntityOperator.OR);
        addToEntityCondMap(delegator, entityCondMap, entityName, cond);
    }


    public static EntityFindOptions getCommonEfo() {
        // TODO: REVIEW: these were from the stock ofbiz groovy
        //return new EntityFindOptions(true, EntityFindOptions.TYPE_SCROLL_INSENSITIVE, EntityFindOptions.CONCUR_READ_ONLY, true);
        return new EntityFindOptions(true, EntityFindOptions.TYPE_SCROLL_INSENSITIVE, EntityFindOptions.CONCUR_READ_ONLY, true);
    }
    
    /******************************************************/
    /* Mid-Level handler callbacks */
    /******************************************************/
    // DEV NOTE: these handlers were added before I migrated the high-level export methods, since which they've become less useful...
    
    public void handleBegin() throws GenericEntityException, IOException {
    }
    
    public void handleFileBegin(PrintWriter writer) throws GenericEntityException, IOException {
        this.setWriter(writer);
    }
    
    public void handleNewRecordName(String entityName) {
        this.currentRecordName = entityName;
    }
    
    public void handleEntityQueryBegin(ModelEntity modelEntity, ListIterator<GenericValue> values) throws GenericEntityException, IOException {
        this.setCurrentRecordModel(modelEntity);
    }
    
    /**
     * Subclass should override if need to handle visitor; default impl ignores visitor.
     */
    protected int handleRecord(CmsEntityVisitor visitor, GenericValue value) throws Exception {
        assert(visitor == null);
        return handleRecordCommon(value);
    }
    
    public int handleRecordCommon(GenericValue value) throws Exception {
        // SPECIAL: CmsMedia
        if (isMediaContentRecord(value)) {
            return handleMediaContentRecord(value);
        }
        
        // SPECIAL: workaround for CmsProcessViewMapping filter failure
        if (doSpecialProcessViewMappingFilter && "CmsProcessMapping".equals(value.getEntityName())) {
            seenCmsProcessMappingIds.add(value.getString("processMappingId"));
        }
        int numberWritten = 0;
        if (this.isIncludeContentRefs()) {
            if (currentContentIdFieldNames == null) {
                throw new IllegalStateException("handleRecord was called without setCurrentRecordModel");
            } else if (!currentContentIdFieldNames.isEmpty()) { // NOTE: automatically empty if this is Content itself
                // SCIPIO: export related Content, DataResource, and other linked records
                for(String fieldName : currentContentIdFieldNames) {
                    String contentId = value.getString(fieldName);
                    if (UtilValidate.isNotEmpty(contentId) && !hasSeenContentId(contentId)) {
                        registerContentId(contentId);
                        if (getRecordGrouping() == RecordGrouping.ENTITY_TYPE) {
                            ; // do nothing, we must handle this separately
                        } else {
                            for(GenericValue relatedValue : getContentAndRelatedValues(contentId)) {
                                writeOut(relatedValue);
                                numberWritten++;
                            }
                        }
                    }
                }
            }
        }
        writeOut(value);
        numberWritten++;
        return numberWritten;
    }
    
    public void writeOut(GenericValue value) throws GenericEntityException, IOException {
        value.writeXmlText(writer, "", entityCdataFields.get(value.getEntityName())); // NOTE: 3rd parameter is a SCIPIO patch
    }
    
    public void handleEntityQueryEnd(ListIterator<GenericValue> values) throws GenericEntityException, IOException {
        this.setCurrentRecordModel(null);
    }
    
    public void handleFileEnd() throws GenericEntityException, IOException {
        if (isMultiFile()) {
            this.setWriter(null);
        }
    }
    
    public void handleEnd() throws GenericEntityException, IOException {
        this.setWriter(null);
    }

    /******************************************************/
    /* High-Level Execution and Implementations */
    /******************************************************/
    
    // Public execution methods
    
    public static class ExecResult {
        protected List<String> errorMsgs = new ArrayList<>();
        protected List<String> resultMsgs = new ArrayList<>();
        protected int numberWritten;

        protected void addMsg(String msg) { resultMsgs.add(msg); }
        protected void addErrorMsg(String msg) { errorMsgs.add(msg); }
        
        public boolean isSuccess() { return errorMsgs.isEmpty(); }
        public List<String> getErrorMessages() { return errorMsgs; }
        public List<String> getResultMessages() { return resultMsgs; }
        public int getNumberWritten() { return numberWritten; }
    }
    
    /**
     * Executes export to the writer, IF single-writer output is supported.
     */
    public ExecResult executeExport(PrintWriter writer) throws Exception {
         // SCIPIO: suspend existing trans to prevent screen render aborts
         Transaction suspendedTrans = null;
         try {
             suspendedTrans = suspendTrans();
             return executeExportInternal(writer);
         } finally {
             resumeTrans(suspendedTrans);
         }
    }
    
    public ExecResult executeExport(Writer writer) throws Exception {
        Transaction suspendedTrans = null;
        try {
            suspendedTrans = suspendTrans();
            return executeExportInternal(new PrintWriter(writer));
        } finally {
            resumeTrans(suspendedTrans);
        }
    }
    
    public ExecResult executeExport(File outFile) throws Exception {
        Transaction suspendedTrans = null;
        try {
            suspendedTrans = suspendTrans();
            return executeExportInternal(outFile);
        } finally {
            resumeTrans(suspendedTrans);
        }
    }


    // Real/Internal Implementations
    
    protected abstract ExecResult executeExportInternal(PrintWriter writer) throws Exception;
    protected abstract ExecResult executeExportInternal(File outFile) throws Exception;
    
    protected EntityListIterator doMainEntityQuery(ModelEntity me) throws GenericEntityException {
        return delegator.find(me.getEntityName(), getEffectiveEntityCond(me.getEntityName(), me.getNoAutoStamp()), null, null, me.getPkFieldNames(), getMainEfo());
    }
    
    protected <T extends ListIterator<GenericValue>> T closeIteratorSafe(T values) {
        if (values instanceof EntityListIterator) {
            try {
                ((EntityListIterator) values).close();
            } catch(Exception e) {
                Debug.logError(e, LOG_PREFIX+"Error closing EntityListIterator: " + e.getMessage(), module);
            }
        }
        return null;
    }
    
    protected Transaction suspendTrans() throws GenericTransactionException {
        try {
            return (useTrans && this.newTrans) ? TransactionUtil.suspend() : null;
        } catch (GenericTransactionException e) {
            Debug.logError(e, LOG_PREFIX+"Error suspending transaction: " + e.getMessage(), module);
            throw e; 
        }
    }
    
    protected boolean beginTrans() throws GenericTransactionException {
        return useTrans ? TransactionUtil.begin(this.transTimeout) : false;
    }
    
    protected void resumeTrans(Transaction trans) throws GenericTransactionException {
        if (trans != null) {
            try {
                TransactionUtil.resume(trans);
            } catch (GenericTransactionException e) {
                Debug.logError(e, LOG_PREFIX+"Error resuming suspended transaction: " + e.getMessage(), module);
            }
        }
    }
    
    protected ModelEntity resolveModelEntity(String entityName) throws GenericEntityException {
        if ("CmsMedia".equals(entityName)) entityName = "Content";
        ModelEntity me = delegator.getModelReader().getModelEntity(entityName);
        if (me instanceof ModelViewEntity) throw new IllegalArgumentException("passed entity name was a View entity - not supported by CMS exporter: " + entityName);
        return me;
    }
    
    protected boolean isMediaContentRecord(GenericValue value) {
        // TODO: REVIEW: check
        return "Content".equals(value.getEntityName()) && "SCP_MEDIA".equals(value.getString("contentTypeId"));
    }
    
    protected int handleMediaContentRecord(GenericValue content) throws GenericEntityException, IOException {
        int numberWritten = 0;
        String contentId = content.getString("contentId");

        if (UtilValidate.isNotEmpty(content.getString("dataResourceId"))) {
            numberWritten += handleMediaDataResourceRecord(content.getRelatedOne("DataResource", false));
        }
        
        writeOut(content);
        numberWritten++;
        
        for(GenericValue attr : content.getRelated("ContentAttribute", null, null, false)) {
            writeOut(attr);
            numberWritten++;
        }
        
        List<GenericValue> assocList = delegator.findList("ContentAssoc", 
                EntityCondition.makeCondition("contentId", contentId), null, null, null, false);
        for(GenericValue assoc : assocList) {
            GenericValue contentTo = assoc.getRelatedOne("ToContent", false);
            if (!isMediaExportVariants() && "SCP_MEDIA_VARIANT".equals(contentTo.getString("contentTypeId"))) {
                continue;
            }
            numberWritten += handleMediaContentRecord(contentTo);
            writeOut(assoc);
            numberWritten++;
        }

        return numberWritten;
    }
    
    protected int handleMediaDataResourceRecord(GenericValue dataRes) throws GenericEntityException, IOException {
        int numberWritten = 0;
        
        writeOut(dataRes);
        numberWritten++;
        
        for(GenericValue attr : dataRes.getRelated("DataResourceAttribute", null, null, false)) {
            writeOut(attr);
            numberWritten++;
        }
        
        GenericValue elecText = dataRes.getRelatedOne("ElectronicText", false);
        if (elecText != null) {
            writeOut(elecText);
            numberWritten++;
        }
        
        SpecDataResEntityInfo specDataResInfo = SpecDataResEntityInfo.fromDataResource(dataRes);
        if (specDataResInfo != null) {
            GenericValue specDataRes = specDataResInfo.getMediaDataResourceFromDataResource(dataRes, false);
            if (specDataRes != null) {
                writeOut(specDataRes);
                numberWritten++;
            }
        }

        return numberWritten;
    }
    
    protected static boolean crossedInterval(int curCount, int lastCount, int interval) {
        // treat first as crossed
        if (lastCount == 0) return true; 
        
        // round up lastCount to next interval
        int boundary = ((lastCount / interval) * interval) + interval; // (integer div)
        
        // did we reach it?
        return (curCount >= boundary);
    }
    
    
    // TODO: REVIEW: this class design is poor, I'm trying to figure it out as we go along
    
    /**
     * Generic worker, doesn't implement any executeExport method, all implementation in CmsDataExportWorker.
     */
    public static class GenericWorker extends CmsDataExportWorker {
        protected GenericWorker(CommonWorkerArgs<?> args) throws IllegalArgumentException { super(args); }
        protected GenericWorker(CmsDataExportWorker other, Delegator delegator) { super(other, delegator); }
        @Override public CmsDataExportWorker cloneWorkerNewState(Delegator delegator) { return new GenericWorker(this, delegator);}
        
        @Override public boolean isMultiFile() { return false; }
        @Override protected RecordGrouping getRecordGrouping() { return RecordGrouping.NONE; }
        @Override public ExecResult executeExportInternal(PrintWriter writer) throws Exception { throw new UnsupportedOperationException(); }
        @Override public ExecResult executeExportInternal(File outFile) throws Exception { throw new UnsupportedOperationException(); }
    }
    
    /**
     * Single file exporter.
     */
    public static class SingleFileWorker extends CmsDataExportWorker {
        protected final RecordGrouping recordGrouping;
        
        protected SingleFileWorker(CommonWorkerArgs<?> args)
                throws IllegalArgumentException {
            super(args);
            this.recordGrouping = args.getRecordGrouping() != null ? args.getRecordGrouping() : RecordGrouping.getDefault();
        }
        
        protected SingleFileWorker(SingleFileWorker other, Delegator delegator) {
            super(other, delegator);
            this.recordGrouping = other.recordGrouping;
        }
        @Override
        public CmsDataExportWorker cloneWorkerNewState(Delegator delegator) {
            return new SingleFileWorker(this, delegator);
        }

        @Override
        public RecordGrouping getRecordGrouping() {
            return recordGrouping;
        }

        @Override
        public boolean isMultiFile() {
            return false;
        }

        @Override
        protected ExecResult executeExportInternal(File outFile) throws Exception {
            PrintWriter writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(outFile), "UTF-8")));
            try {
                return executeExportInternal(writer);
            } finally {
                try {
                    writer.close();
                } catch(Exception e) {
                    ;
                }
            }
        }
        
        @Override
        protected ExecResult executeExportInternal(PrintWriter writer) throws Exception {
            ExecResult result = new ExecResult();
            
            writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            writer.println("<entity-engine-xml>");
  
            int numberWritten = 0;
            boolean beganTransaction;
            
            PrintWriter origWriter = writer;
            StringWriter tempWriter = null;
            boolean specialTypeGrouping = (getRecordGrouping() == RecordGrouping.ENTITY_TYPE) && this.isIncludeContentRefs();
            if (specialTypeGrouping) {
                // FIXME: if we need to output by entity type in a single file, we have to output Content records
                // BEFORE everything else.
                // this causes a performance problem because we have to buffer everything before in memory.
                // there is no other real solution available currently.
                tempWriter = new StringWriter();
                writer = new PrintWriter(tempWriter);
            }

            this.handleBegin();
            this.handleFileBegin(writer);
            
            for(String curEntityName : getTargetEntityNames()) {
                numberWritten += exportEntityRecords(null, curEntityName, numberWritten);
            }
            
            if (specialTypeGrouping) {
                // we write the Content records to the origWriter so they come before
                // everything else; then add our string buffer
                writer.flush();
                writer = origWriter;
                this.setWriter(writer);

                Map<String, List<GenericValue>> valuesByEntity = Collections.emptyMap();
                beganTransaction = false;
                try {
                    beganTransaction = beginTrans();
                    valuesByEntity = getContentAndRelatedValuesByEntity(getSeenContentIds());
                    TransactionUtil.commit(beganTransaction);
                } catch(Exception e) {
                    handleError(e, beganTransaction);
                }
                
                for(Map.Entry<String, List<GenericValue>> entry : valuesByEntity.entrySet()) {
                    String curEntityName = entry.getKey();
                    List<GenericValue> entityValues = entry.getValue();
                    if (entityValues == null || entityValues.isEmpty()) continue;
                    numberWritten += exportEntityRecords(null, curEntityName, entityValues.listIterator(), numberWritten);
                }
                // append the temp buffer
                writer.append(tempWriter.getBuffer());
            }
            
            if (getTargetSpecialEntityNames().contains("CmsMedia")) {
                // FIXME: non-integrated solution for now
                numberWritten += exportEntityRecords(null, "CmsMedia", numberWritten);
            }
            
            this.handleFileEnd();
            this.handleEnd();
            
            writer.println("</entity-engine-xml>");
            Debug.logInfo(LOG_PREFIX+"Total records written from all entities: " + numberWritten, module);
            result.numberWritten = numberWritten;
            return result;
        }
        
        protected int exportEntityRecords(CmsEntityVisitor visitor, String curEntityName, int numberWritten) throws Exception {
            return exportEntityRecords(visitor, curEntityName, null, numberWritten);
        }
        
        protected int exportEntityRecords(CmsEntityVisitor visitor, String curEntityName, ListIterator<GenericValue> values, int numberWritten) throws Exception {
            boolean beganTransaction = false;
            this.handleNewRecordName(curEntityName);
            int curNumberWritten = 0;
            try {
                beganTransaction = beginTrans();
                ModelEntity me = resolveModelEntity(curEntityName);
                
                // FIXME?: hardcoded special case, doesn't really belong here
                if ("CmsMedia".equals(curEntityName) && isMediaExportVariants()) {
                    ListIterator<GenericValue> catValues = null;
                    try {
                        catValues = CmsMediaWorker.findVariantContentAssocTypes(delegator);
                        ModelEntity catme = resolveModelEntity("ContentAssocType");
                        curNumberWritten += handleEntityRecordsCore(null, "ContentAssocType", catme, catValues, numberWritten);
                        numberWritten += curNumberWritten;
                    } finally {
                        catValues = closeIteratorSafe(catValues);
                    }
                }

                if (values == null) {
                    values = doMainEntityQuery(me);
                }

                curNumberWritten += handleEntityRecordsCore(visitor, curEntityName, me, values, numberWritten);
                numberWritten += curNumberWritten;
                
                values = closeIteratorSafe(values);
                TransactionUtil.commit(beganTransaction);
                Debug.logInfo(LOG_PREFIX+"Committed records [" + curEntityName + "]: Main query: " + curNumberWritten + "; Total All: " + numberWritten, module);
            } catch (Exception e) {
                values = closeIteratorSafe(values);
                handleError(e, beganTransaction);
            }
            return curNumberWritten;
        }
        
        protected int handleEntityRecordsCore(CmsEntityVisitor visitor, String curEntityName, ModelEntity me, ListIterator<GenericValue> values, int numberWritten) throws Exception {
            this.handleEntityQueryBegin(me, values);
            GenericValue value = null;
            int curNumberWritten = 0;
            int lastNumberWritten = 0;
            boolean isEntityListIterator = (values instanceof EntityListIterator);
            while ((isEntityListIterator || values.hasNext()) && ((value = values.next()) != null)) {
                int numAdded = this.handleRecord(visitor, value);
                numberWritten += numAdded;
                curNumberWritten += numAdded;
                if (crossedInterval(curNumberWritten, lastNumberWritten, LOG_RECORD_COUNT_INTERVAL)) {
                    Debug.logInfo(LOG_PREFIX+"Records written [" + curEntityName + "]: Main query: " + curNumberWritten + "; Total All: " + numberWritten, module);
                }
                lastNumberWritten = curNumberWritten;
            }
            this.handleEntityQueryEnd(values);
            return curNumberWritten;
        }
        
        protected void handleError(Exception e, boolean beganTransaction) throws Exception {
            String errMsg = "Failure in operation, rolling back transaction";
            Debug.logError(e, LOG_PREFIX+errMsg, module);
            try {
                // only rollback the transaction if we started one...
                TransactionUtil.rollback(beganTransaction, errMsg, e);
            } catch (GenericEntityException e2) {
                Debug.logError(e2, LOG_PREFIX+"Could not rollback transaction: " + e2.toString(), module);
            }
            // after rolling back, rethrow the exception
            throw e;
        }
    }

    /**
     * Special dedicated instance to try to implement the object grouping code.
     * NOTE: this will be several times slower than the others, which makes it bad for large exports.
     * DEV NOTE: may need re-refactoring later, trying to figure this out as we go along.
     */
    public static class ObjGrpSingleFileWorker extends SingleFileWorker {
        protected final boolean includeMajorDeps;
        protected final Set<String> enterEntityNames;
        protected final Set<String> enterMajorEntityNames;
        protected ObjGrpSingleFileWorker(CommonWorkerArgs<?> args) throws IllegalArgumentException {
            super(overrideArgs(args));
            this.includeMajorDeps = args.isIncludeMajorDeps();
            this.enterEntityNames = args.getEffectiveEnterEntityNames();
            this.enterMajorEntityNames = args.getEffectiveEnterMajorEntityNames();
        }
        private static CommonWorkerArgs<?> overrideArgs(CommonWorkerArgs<?> args) { // java kludge
            args.setRecordGrouping(RecordGrouping.MAJOR_OBJECT);
            return args;
        }
        protected ObjGrpSingleFileWorker(ObjGrpSingleFileWorker other, Delegator delegator) {
            super(other, delegator);
            this.includeMajorDeps = other.includeMajorDeps;
            this.enterEntityNames = other.enterEntityNames;
            this.enterMajorEntityNames = other.enterMajorEntityNames;
        }
        @Override
        public ObjGrpSingleFileWorker cloneWorkerNewState(Delegator delegator) {
            return new ObjGrpSingleFileWorker(this, delegator);
        }

        @Override
        public RecordGrouping getRecordGrouping() { return RecordGrouping.MAJOR_OBJECT; }

        @Override
        protected ExecResult executeExportInternal(PrintWriter writer) throws Exception {
            ExecResult result = new ExecResult();
            
            writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
            writer.println("<entity-engine-xml>");
  
            int numberWritten = 0;

            this.handleBegin();
            this.handleFileBegin(writer);
            
            Set<String> targetMajorEntityNames = entityInfo.filterMajorCmsEntityNames(this.getTargetEntityNames());
            
            // SPECIAL: in record-grouping mode, if CMS_PGSPCMAP_PRIMARY was targeted and CmsPage
            // was included, we'll remove CmsProcessMapping from the entities to target because already included in the CmsPage visiting
            // and this simplifies to PK filter
            // NOTE: order of entities (CmsPage first) may also affect results of this
            // FIXME: this check is flawed but just working enough for now
            if (targetMajorEntityNames.contains("CmsPage") && targetMajorEntityNames.contains("CmsProcessMapping")) {
                if (UtilValidate.isNotEmpty(this.pmpsMappingTypeId) && !"CMS_PGSPCMAP_STD".equals(this.pmpsMappingTypeId)) {
                    targetMajorEntityNames.remove("CmsProcessMapping");
                }
            } 
            
            ObjGrpEntityVisitor visitor = new ObjGrpEntityVisitor(delegator);
            visitor.setEnterContent(this.isIncludeContentRefs()); // usually do enter content records, unless disabled
            visitor.setEnterMajor(includeMajorDeps); // usually don't enter deep deps, unless enabled
            
            visitor.setEnterEntityNames(enterEntityNames);
            visitor.setEnterMajorEntityNames(enterMajorEntityNames);
            
            for(String curEntityName : targetMajorEntityNames) {
                numberWritten += exportEntityRecords(visitor, curEntityName, numberWritten);
            }
            
            if (getTargetSpecialEntityNames().contains("CmsMedia")) {
                // FIXME: non-integrated solution for now (NOTE: do not pass visitor!)
                numberWritten += exportEntityRecords(null, "CmsMedia", numberWritten);
            }
            
            this.handleFileEnd();
            this.handleEnd();
            
            writer.println("</entity-engine-xml>");
            Debug.logInfo(LOG_PREFIX+"Total records written from all entities: " + numberWritten, module);
            result.numberWritten = numberWritten;
            return result;
        }

        @Override
        protected int handleRecord(CmsEntityVisitor visitorObj, GenericValue value) throws Exception {
            if (visitorObj == null) return handleRecordCommon(value);
            
            ObjGrpEntityVisitor visitor = (ObjGrpEntityVisitor) visitorObj;
            visitor.setNumberWritten(0);
            
            DataObjectWorker<?> dataObjWorker = CmsObjectRegistry.getEntityDataObjectWorkerAlways(value.getEntityName());
            CmsMajorObject majorDataObj = (CmsMajorObject) dataObjWorker.makeFromValue(value);
            
            // begin visiting
            majorDataObj.acceptEntityDepsVisitor(visitor, null, null, majorDataObj);
            
            return visitor.getNumberWritten();
        }
    }
    
    /**
     * Object grouping entity visitor implementation.
     * TODO: REVIEW
     */
    protected class ObjGrpEntityVisitor extends CmsEntityVisit.AbstractCmsEntityVisitor {
        protected ObjGrpVisitContext visitContext;
        protected int numberWritten = 0;
        protected Set<String> seenMajorEntities = new HashSet<>(); // entityName + "@" + PK
        // NOTE: seenAllEntities could get very big, so we only use seenMajorEntities in production
        protected Set<String> seenAllEntities = CmsUtil.debugOn() ? new HashSet<String>() : null;
        
        public ObjGrpEntityVisitor(Delegator delegator) {
            super(delegator);
            this.visitContext = new ObjGrpVisitContext();
        }

        @Override
        public ObjGrpVisitContext getVisitContext() { return visitContext; }
        
        public int getNumberWritten() { return numberWritten; }
        public void setNumberWritten(int numberWritten) { this.numberWritten = numberWritten; }

        protected boolean hasSeenMajorEntity(String entityName, String pk) {
            final String key = entityName + "@" + pk;
            if (seenMajorEntities.contains(key)) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo(LOG_PREFIX+"DUP: Prevented duplicate major entity: " + key, module);
                }
                return true;
            } else {
                return false;
            }
        }
        
        protected void registerMajorEntity(String entityName, String pk) {
            seenMajorEntities.add(entityName + "@" + pk);
        }
        
        // PRE-LOOKUP FILTERS
        
        @Override
        protected boolean shouldEnterMajor(VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) {
            // Here we check if the PKs of given major entities were already visited (one relations)
            // WARN: we can only check seen entities in pre-filter for ONE relations
            if (relation.getDataRelType().isOneAny()) {
                String pk = relation.extractRelOneEntitySingleFieldPk(relValue);
                if (pk == null) return false; // won't be a value, can stop early
                if (hasSeenMajorEntity(relation.getRelEntityName(), pk)) return false;
            }
            return true;
        }

        @Override
        protected boolean shouldEnterContent(VisitRelation relation, GenericValue relValue,
                CmsMajorObject majorDataObj) {
            boolean seen = hasSeenContentId(relation.extractRelOneEntitySingleFieldPk(relValue));
            if (CmsUtil.verboseOn()) {
                if (seen) {
                    Debug.logInfo(LOG_PREFIX+"DUP: Prevented duplicate Content record: " + relation.extractRelOneEntitySingleFieldPk(relValue), module);
                }
            }
            return !seen;
        }
        
        // POST-LOOKUP FILTERS
        
        @Override
        public boolean shouldEnter(GenericValue value, VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) {
            // Here we check if the PKs of given major entities were already visited (many & self relations)
            // NOTE: we only need to check MANY relations here because one covered in pre-filter, and
            // as a special case must also check SELF relation (which will be major at same time)
            if (relation.isMajor() && (relation.getDataRelType().isMany() || relation.getDataRelType().isSelf())) {
                if (hasSeenMajorEntity(value.getEntityName(), value.getPkShortValueString())) return false;
            }
            return true;
        }

        @Override
        public void visit(GenericValue value, VisitRelation relation, GenericValue relValue, CmsMajorObject majorDataObj) throws Exception {
            visitRecordOnly(value, relation, relValue, majorDataObj);
            visitWriteOnly(value, relation, relValue, majorDataObj);
        }
        
        @Override
        public void visitRecordOnly(GenericValue value, VisitRelation relation, GenericValue relValue,
                CmsMajorObject majorDataObj) throws Exception {
            if (relation != null) { // WARN: some case like DataResource passing null for this for now...
                if (relation.isContentPrimary()) { // WARN: check will be insufficient later, for now assume this is main Content entity only
                    if (CmsUtil.verboseOn()) {
                        if (hasSeenContentId(value.getString("contentId"))) {
                            Debug.logError(LOG_PREFIX+"Unexpected duplicate contentId in visit method: " + value.getString("contentId"), module);
                        }
                    }
                    registerContentId(value.getString("contentId"));
                } else if (relation.isMajor()) { // NOTE: includes isSelf() at major boundaries
                    if (CmsUtil.verboseOn()) {
                        if (hasSeenMajorEntity(value.getEntityName(), value.getPkShortValueString())) {
                            Debug.logError(LOG_PREFIX+"Unexpected duplicate major entity in visit method: " + value.getEntityName() + "@" + value.getPkShortValueString(), module);
                        }
                    }
                    registerMajorEntity(value.getEntityName(), value.getPkShortValueString());
                }
            }
            if (seenAllEntities != null) {
                // this is for debugging purposes only, we don't want this on large exports
                final String key = value.getEntityName() + "@" + value.getPkShortValueString();
                if (seenAllEntities.contains(key)) {
                    Debug.logError(LOG_PREFIX+"Unexpected duplicate entity in visit method: " + key, module);
                } else {
                    seenAllEntities.add(key);
                }
            }
        }

        @Override
        public void visitWriteOnly(GenericValue value, VisitRelation relation, GenericValue relValue,
                CmsMajorObject majorDataObj) throws Exception {
            // NOTE: the numberWritten is never used excess as statistic so don't consider part of state.
            writeOut(value);
            numberWritten++;
        }

        public class ObjGrpVisitContext extends CmsEntityVisit.AbstractCmsEntityVisitor.AbstractVisitContext {
            @Override
            public boolean isExportFilesAsTextData() {
                return exportFilesAsTextData;
            }
        }
    }

    /**
     * Multi-file directory exporter.
     */
    public static class MultiFileWorker extends CmsDataExportWorker {
        protected int maxRecordsPerFile;
        
        protected MultiFileWorker(CommonWorkerArgs<?> args) throws IllegalArgumentException {
            super(args);
            this.maxRecordsPerFile = args.getMaxRecordsPerFile();
        }
        protected MultiFileWorker(MultiFileWorker other, Delegator delegator) {
            super(other, delegator);
        }
        @Override
        public CmsDataExportWorker cloneWorkerNewState(Delegator delegator) {
            return new MultiFileWorker(this, delegator);
        }

        @Override
        public RecordGrouping getRecordGrouping() { return RecordGrouping.ENTITY_TYPE; }
        @Override
        public boolean isMultiFile() { return true; }

        @Override
        protected ExecResult executeExportInternal(PrintWriter writer) {
            throw new UnsupportedOperationException("MultiFileWorker can't export to a single Writer");
        }

        @Override
        protected ExecResult executeExportInternal(File outdir) throws Exception {
            ExecResult result = new ExecResult();
            boolean beganTransaction;
            
            if (outdir.isDirectory() && outdir.canWrite()) {
                // SCIPIO: we start at 4 so that Content, DataResource & ElectronicText will get loaded first,
                // and leaving space for others...
                int fileNumber = 10;
                this.handleBegin();
                
                for(String curEntityName : getTargetEntityNames()) {
                    fileNumber += exportEntityRecords(null, curEntityName, result, fileNumber, outdir, true);
                }
                
                Map<String, List<GenericValue>> valuesByEntity = Collections.emptyMap();
                beganTransaction = false;
                try {
                    beganTransaction = beginTrans();
                    valuesByEntity = getContentAndRelatedValuesByEntity(getSeenContentIds());
                    TransactionUtil.commit(beganTransaction);
                } catch(Exception e) {
                    handleError(e, "Error when querying additional Content(/DataResource/ElectronicText) records: " + e.getMessage(), result, beganTransaction);
                }
                
                // SCIPIO: now do the Content values
                fileNumber = 1;
                for(Map.Entry<String, List<GenericValue>> entry : valuesByEntity.entrySet()) {
                    if (entry.getValue().isEmpty()) continue;
                    fileNumber += exportEntityRecords(null, entry.getKey(), entry.getValue().listIterator(), result, fileNumber, outdir, false);
                }
                
                if (getTargetSpecialEntityNames().contains("CmsMedia")) {
                    // FIXME: non-flexible solution, but it's not that bad for now
                    fileNumber += exportEntityRecords(null, "CmsMedia", result, fileNumber, outdir, true);
                }
                
                this.handleEnd();
            }

            return result;
        }

        protected int exportEntityRecords(CmsEntityVisitor visitor, String curEntityName, ExecResult result, int fileNumber, File outdir, boolean doHooks) throws Exception {
            return exportEntityRecords(visitor, curEntityName, null, result, fileNumber, outdir, doHooks);
        }
        
        protected int exportEntityRecords(CmsEntityVisitor visitor, String curEntityName, ListIterator<GenericValue> values, 
                ExecResult result, int fileNumber, File outdir, boolean doHooks) throws Exception {
            this.handleNewRecordName(curEntityName);
            boolean beganTransaction = false;
            try {
                beganTransaction = beginTrans();

                ModelEntity me = resolveModelEntity(curEntityName);
                
                // FIXME?: hardcoded special case, doesn't really belong here
                if ("CmsMedia".equals(curEntityName) && isMediaExportVariants()) {
                    ListIterator<GenericValue> catValues = null;
                    try {
                        catValues = CmsMediaWorker.findVariantContentAssocTypes(delegator);
                        ModelEntity catme = resolveModelEntity("ContentAssocType");
                        handleEntityRecordsCore(null, "ContentAssocType", catme, catValues, result, fileNumber, outdir, doHooks);
                    } finally {
                        catValues = closeIteratorSafe(catValues);
                    }
                }
                
                if (values == null) {
                    values = doMainEntityQuery(me);
                }
                
                handleEntityRecordsCore(visitor, curEntityName, me, values, result, fileNumber, outdir, doHooks);
                
                values = closeIteratorSafe(values);
                // only commit the transaction if we started one... this will throw an exception if it fails
                TransactionUtil.commit(beganTransaction);
            } catch (Exception e) {
                values = closeIteratorSafe(values);
                handleEntityWriteError(e, result, beganTransaction, curEntityName, fileNumber);
            }
            return 1;
        }
        
        // TODO: REVIEW: can't remember why doHooks was here...
        protected void handleEntityRecordsCore(CmsEntityVisitor visitor, String curEntityName, ModelEntity me, ListIterator<GenericValue> values, 
                ExecResult result, int fileNumber, File outdir, boolean doHooks) throws Exception {
            int numberWritten = 0;
            String fileName = (maxRecordsPerFile > 0) ? UtilFormatOut.formatPaddedNumber((long) fileNumber, 3) + "_" : "";
            fileName = fileName + curEntityName;
            
            this.handleEntityQueryBegin(me, values);
            
            boolean isFirst = true;
            PrintWriter writer = null;
            int fileSplitNumber = 1;
            GenericValue value;
            boolean isEntityListIterator = (values instanceof EntityListIterator);
            while ((isEntityListIterator || values.hasNext()) && ((value = values.next()) != null)) {
                //Don't bother writing the file if there's nothing
                //to put into it
                if (isFirst) {
                    writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(outdir, fileName +".xml")), "UTF-8")));
                    writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                    writer.println("<entity-engine-xml>");
                    isFirst = false;
                    this.handleFileBegin(writer);
                }
                
                numberWritten += this.handleRecord(visitor, value);

                // split into small files
                if (maxRecordsPerFile > 0 && (numberWritten % maxRecordsPerFile == 0)) {
                    fileSplitNumber++;
                    this.handleFileEnd();
                    // close the file
                    writer.println("</entity-engine-xml>");
                    writer.close();

                    // create a new file
                    String splitNumStr = UtilFormatOut.formatPaddedNumber((long) fileSplitNumber, 3);
                    writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(new File(outdir, fileName + "_" + splitNumStr +".xml")), "UTF-8")));
                    writer.println("<?xml version=\"1.0\" encoding=\"UTF-8\"?>");
                    writer.println("<entity-engine-xml>");
                    this.handleFileBegin(writer);
                }

                // FIXME: MODULUS DOES NOT WORK
                if (numberWritten % 500 == 0 || numberWritten == 1) {
                    Debug.logInfo(LOG_PREFIX+"Records written [" + curEntityName + "]: " + numberWritten, module);
                }
            }
            
            this.handleEntityQueryEnd(values);
            
            if (writer != null) {
                this.handleFileEnd();
                writer.println("</entity-engine-xml>");
                writer.close();
                String thisResult = "[" + fileNumber + "] [" + numberWritten + "] " + curEntityName + " wrote " + numberWritten + " records";
                Debug.logInfo(LOG_PREFIX+thisResult, module);
                result.addMsg(thisResult);
            } else {
                String thisResult = "[" + fileNumber + "] [---] " + curEntityName + " has no records, not writing file";
                Debug.logInfo(LOG_PREFIX+thisResult, module);
                result.addMsg(thisResult);
            }
        }
        
        private void handleError(Exception e, String errorMsg, ExecResult result, boolean beganTransaction) throws Exception {
            Debug.logError(e, LOG_PREFIX+errorMsg, module);
            result.addMsg(errorMsg);
            try {
                TransactionUtil.rollback(beganTransaction, errorMsg, e);
            } catch (GenericEntityException e2) {
                Debug.logError(e2, LOG_PREFIX+"Could not rollback transaction: " + e2.toString(), module);
            }
            throw e;
        }
        
        private void handleEntityWriteError(Exception e, ExecResult result, boolean beganTransaction, String curEntityName, int fileNumber) throws Exception {
            handleError(e, "[" + fileNumber + "] Error while writing " + curEntityName + ": " + e.getMessage(), result, beganTransaction);
        }
    }

}

