package com.ilscipio.scipio.cms.template;

import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.ScriptUtil;
import org.ofbiz.base.util.Scriptlet;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.model.AbstractModelAction;
import org.ofbiz.widget.model.ModelScreen;
import org.ofbiz.widget.model.ScreenFactory;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.control.CmsControlUtil;
import com.ilscipio.scipio.cms.data.CmsDataException;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.data.CmsEntityVisit;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.CmsEntityVisitor;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelation;
import com.ilscipio.scipio.cms.data.CmsEntityVisit.VisitRelations;
import com.ilscipio.scipio.cms.data.CmsMajorObject;
import com.ilscipio.scipio.cms.data.CmsObjectCache;
import com.ilscipio.scipio.cms.data.CmsObjectCache.CacheEntry;

/**
 * Script template
 * <p>
 * TODO: 2016: currently UI and such only supports *.groovy locations and groovy type.
 * can easily support more...
 */
public class CmsScriptTemplate extends CmsComplexTemplate implements CmsMajorObject  {

    private static final long serialVersionUID = -1589382218994735791L;
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Fields recognized by service but not physically present in entity.
     */
    public static final Set<String> virtualFields = CmsTemplate.virtualFields;
    
    private static final CmsObjectCache<CmsScriptTemplate> idCache = CmsObjectCache.getGlobalCache("cms.template.script.id");
    private static final CmsObjectCache<CmsScriptTemplate> nameCache = CmsObjectCache.getGlobalCache("cms.template.script.name");

    // WARN: if ever changed, there is code that does not check this bool...
    public static final boolean STANDALONE_DEFAULT = true;

    private static final EntityCondition standaloneCond;
    private static final EntityCondition notStandaloneCond;
    static {
        if (STANDALONE_DEFAULT) {
            standaloneCond = EntityCondition.makeCondition(EntityCondition.makeCondition("standalone", "Y"),
                    EntityOperator.OR,
                    EntityCondition.makeCondition("standalone", null));
            notStandaloneCond = EntityCondition.makeCondition("standalone", "N");
        } else {
            standaloneCond = EntityCondition.makeCondition("standalone", "Y");
            notStandaloneCond = EntityCondition.makeCondition(EntityCondition.makeCondition("standalone", "N"),
                    EntityOperator.OR,
                    EntityCondition.makeCondition("standalone", null));
        }
    }
    
    private static final String processorLocation = UtilProperties.getPropertyValue("cms.properties", "contentprocessor.script.location");

    private static final ScriptExecutor processorScriptExecutor = makeProcessorExecutor();
    
    private ScriptExecutor executor; // 2016: dedicated executor object
    private CmsScriptTemplateAssoc assoc; // 2016: backreference to association
    
    protected CmsScriptTemplate(GenericValue entity) {
        super(entity);
    }
    
    protected CmsScriptTemplate(GenericValue entity, CmsScriptTemplateAssoc assoc) {
        super(entity);
        this.assoc = assoc;
    }

    public CmsScriptTemplate(Delegator delegator, Map<String, ?> fields) {
        super(delegator, checkFields(fields, true));
    }
    
    protected CmsScriptTemplate(Delegator delegator, Map<String, ?> fields, CmsScriptTemplateAssoc assoc) {
        super(delegator, checkFields(fields, true));
        this.assoc = assoc;
    }
    
    protected CmsScriptTemplate(CmsScriptTemplate other, Map<String, Object> copyArgs) {
        super(other, copyArgs);
    }
    
    @Override    
    public void update(Map<String, ?> fields, boolean setIfEmpty) {
        super.update(checkFields(fields, false), setIfEmpty);
    }
    
    @Override
    public CmsScriptTemplate copy(Map<String, Object> copyArgs) throws CmsException {
        return new CmsScriptTemplate(this, copyArgs);
    }

    public static CmsScriptTemplate createUpdateScriptTemplate(Delegator delegator, Map<String, ?> fields, 
            GenericValue userLogin, boolean store) {
        Map<String, Object> scriptMap = new HashMap<>(fields);
        CmsScriptTemplate scriptTemplate;
        String scriptTemplateId = (String) fields.get("scriptTemplateId");
        if (UtilValidate.isNotEmpty(scriptTemplateId)) {
            scriptTemplate = CmsScriptTemplate.getWorker().findByIdAlways(delegator, scriptTemplateId, false);
            scriptTemplate.update(scriptMap, true);
        } else {
            scriptMap.put("createdBy", userLogin.get("userLoginId"));
            scriptTemplate = new CmsScriptTemplate(delegator, scriptMap);
        }
        if (store) {
            scriptTemplate.store();
        }
        return scriptTemplate;
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
        this.getExecutor();
        // NOTE: un-intuitively, we must do the preload for the assoc, because instances store us
        // rather than the assoc itself!
        preloadWorker.preload(this.assoc);
    }
    
    protected static <T> Map<String, T> checkFields(Map<String, T> fields, boolean isNew) throws CmsException {
        if (isNew || fields.containsKey("standalone")) {
            if (UtilValidate.isEmpty((String) fields.get("standalone"))) {
                UtilGenerics.<String, Object> checkMap(fields).put("standalone", STANDALONE_DEFAULT ? "Y" : "N");
            }
        }
        return fields;
    }
    
    @Override
    public List<CmsAttributeTemplate> getAttributeTemplates() { 
        return null;
    }

    @Override
    public void addAttributeTemplate(CmsAttributeTemplate template) {
        throw new UnsupportedOperationException();
    }
    
    public String getScriptLang() { // 2016: new
        return entity.getString("scriptLang");
    }
    
    public String getResolvedScriptLang() {
        try {
            ScriptExecutor executor = this.getExecutor();
            return executor.getScriptLang().getName();
        } catch (Exception e) {
            Debug.logError(e, "Cms: Error determining script language for script '" + getId() + "'", module);
            return null;
        }
    }
    
    public Boolean getStandaloneBoolean() { // 2016: new
        return entity.getBoolean("standalone");
    }
    
    public boolean isStandalone() {
        Boolean standalone = getStandaloneBoolean();
        return standalone != null ? standalone : STANDALONE_DEFAULT;
    }
    
    public static EntityCondition getStandaloneCond() {
        return standaloneCond;
    }
    
    public static EntityCondition getNotStandaloneCond() {
        return notStandaloneCond;
    }
    
    public boolean isOrphan() {
        try {
            return UtilValidate.isEmpty(getDelegator().findByAnd("CmsPageTemplateScriptAssoc", 
                    UtilMisc.toMap("scriptTemplateId", getId()), null, false)) && 
                    UtilValidate.isEmpty(getDelegator().findByAnd("CmsAssetTemplateScriptAssoc", 
                            UtilMisc.toMap("scriptTemplateId", getId()), null, false)) &&
                    UtilValidate.isEmpty(getDelegator().findByAnd("CmsPageScriptAssoc", 
                            UtilMisc.toMap("scriptTemplateId", getId()), null, false));
        } catch (GenericEntityException e) {
            throw new CmsDataException(e);
        }
    }

    public String getQualifiedName() { // 2016: new
        TemplateBodySource tmplBodySrc = getTemplateBodySource();
        String location = tmplBodySrc.getLocation();
        String invokeName = getAssoc() != null ? getAssoc().getInvokeName() : null;
        
        String qualName;
        if (UtilValidate.isNotEmpty(location)) {
            qualName = location;
        } else if (tmplBodySrc.getStoredBody() != null) {
            qualName = "[stored body]";
        } else {
            Debug.logError("Cms: Detected invalid template body source for script template " + getId() + 
                    "; neither location nor stored body exists", module);
            qualName = "[invalid]";
        }
        
        if (UtilValidate.isNotEmpty(invokeName)) {
            qualName += "#" + invokeName;
        }
        return qualName;
    }
    

    @Override
    public int remove() {
        int rowsAffected = 0;
        Delegator delegator = getDelegator();
        try {
            // delete CmsPageTemplateScriptAssoc
            List<GenericValue> scriptAssoc = entity.getRelated("CmsPageTemplateScriptAssoc", null, null, false);
            for (GenericValue scriptValue : scriptAssoc) {
                scriptValue.remove();
                rowsAffected += 1;
            }
            // delete CmsAssetTemplateScriptAssoc
            scriptAssoc = entity.getRelated("CmsAssetTemplateScriptAssoc", null, null, false);
            for (GenericValue scriptValue : scriptAssoc) {
                scriptValue.remove();
                rowsAffected += 1;
            }
            // delete CmsPageScriptAssoc
            scriptAssoc = entity.getRelated("CmsPageScriptAssoc", null, null, false);
            for (GenericValue scriptValue : scriptAssoc) {
                scriptValue.remove();
                rowsAffected += 1;
            }
        } catch (GenericEntityException e) {
            throw makeRemoveException(e);
        }
        String contentId = getTemplateContentId();
        return rowsAffected + super.remove() + removeTemplateBodySourceCommon(delegator, contentId);
    }
    
    public int removeIfOrphan() {
        if (!isStandalone() && isOrphan()) {
            Debug.logInfo("Cms: Deleting script '" + getId() + "' because it is marked as non-standalone (dependent)"
                    + " and has become orphaned", module);
            return super.remove();
        } else {
            return 0;
        }
    }
    
    public ScriptExecutor getExecutor() {
        ScriptExecutor executor = this.executor;
        if (executor == null) {
            try {
                executor = ScriptExecutor.getExecutor(getScriptLang(), getId(), 
                        getTemplateBodySource(), getAssoc() != null ? getAssoc().getInvokeName() : null);
                if (executor == null) {
                    throw new CmsException("Invalid script template");
                }
            } catch(Throwable t) {
                Debug.logError(t, "Cms: Invalid script template: " + getScriptLogRepr(), module);
                executor = ScriptExecutor.getDummyExecutor();
            }
            this.executor = executor;
        }
        return executor;
    }

    public static ScriptExecutor getProcessorScriptExecutor() {
        return processorScriptExecutor;
    }

    private static ScriptExecutor makeProcessorExecutor() {
        ScriptExecutor executor = null;
        if (UtilValidate.isNotEmpty(processorLocation)) {
            try {
                executor = ScriptExecutor.getExecutor("PROCESSOR", TemplateBodySource.fromLocation(processorLocation), null);
            } catch(Throwable t) {
                Debug.logError("Cms: Invalid process script executor for location: " + processorLocation, module);
            }
        }
        return executor != null ? executor : ScriptExecutor.getDummyExecutor();
    }
    
    public static String getProcessorLocation() {
        return processorLocation;
    }
    
    public String getScriptLogRepr() {
        TemplateBodySource tmplBodySrc = getTemplateBodySource();
        if (UtilValidate.isNotEmpty(tmplBodySrc.getLocation())) {
            return "[id: " + getId() + ", location: " + tmplBodySrc.getLocation() + "]";
        } else {
            return "[id: " + getId() + "]";
        }
    }
    
    public CmsScriptTemplateAssoc getAssoc() {
        return assoc;
    }
    
    public Long getInputPosition() {
        return getAssoc().getInputPosition();
    }
    
    public String getInvokeName() {
        return getAssoc().getInvokeName();
    }
    
    public String getAssocId() {
        return getAssoc().getAssocId();
    }
    
    public static abstract class CmsScriptTemplateAssoc extends CmsDataObject {

        private static final long serialVersionUID = -4166919325339588209L;

        protected CmsScriptTemplate scriptTemplate;

        protected CmsScriptTemplateAssoc(GenericValue entity) {
            super(entity);
        }

        public CmsScriptTemplateAssoc(Delegator delegator, Map<String, ?> fields, CmsScriptTemplate scriptTemplate) {
            super(delegator, fields);
            this.scriptTemplate = scriptTemplate;
        }

        protected CmsScriptTemplateAssoc(CmsScriptTemplateAssoc other, Map<String, Object> copyArgs) {
            super(other, copyArgs);
            // NOTE: don't bother clearing out the ID fields here, caller should handle
        }

        @Override    
        public void update(Map<String, ?> fields, boolean setIfEmpty) {
            // here, must ignore scriptTemplateId - set at creation and should never change
            if (fields.containsKey("scriptTemplateId") && UtilValidate.isNotEmpty(getScriptTemplateId())) {
                fields = new HashMap<>(fields);
                fields.remove("scriptTemplateId");
            }
            super.update(fields, setIfEmpty);
        }
        
        @Override
        public abstract CmsScriptTemplateAssoc copy(Map<String, Object> copyArgs);
        
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
            // DO NOT do this way because the preload will (un-intuitively) come from CmsScriptTemplate to us
            //preloadWorker.preload(getScriptTemplate());
        }

        @Override
        public abstract ScriptTemplateAssocWorker<? extends CmsScriptTemplateAssoc> getWorkerInst();

        @Override
        public void store() throws CmsException {
            if (scriptTemplate != null) {
                scriptTemplate.store();
                if (UtilValidate.isEmpty(getScriptTemplateId())) {
                    setScriptTemplateId(scriptTemplate.getId());
                } else {
                    if (!getScriptTemplateId().equals(scriptTemplate.getId())) {
                        throw new CmsException("Error: trying to change the scriptTemplateId of "
                                + "existing script association '" + getId() + "'; cannot be changed once after creation");
                    }
                }
            }
            super.store();
        }

        public String getAssocId() {
            return getId();
        }
        
        public String getScriptTemplateId() {
            return entity.getString("scriptTemplateId");
        }
        
        private void setScriptTemplateId(String scriptTemplateId) {
            entity.setString("scriptTemplateId", scriptTemplateId);
        }
        
        public CmsScriptTemplate getScriptTemplate() {
            CmsScriptTemplate scriptTemplate = this.scriptTemplate;
            final CmsScriptTemplateAssoc assoc = this;
            if (scriptTemplate == null) {
                scriptTemplate = new ScriptTemplateWorker() { // extend the script template worker to pass the assoc to CmsScriptTemplate constructor
                    @Override
                    public CmsScriptTemplate makeFromValue(GenericValue value) throws CmsException {
                        return new CmsScriptTemplate(value, assoc);
                    }

                    @Override
                    public CmsScriptTemplate makeFromFields(Delegator delegator, Map<String, ?> fields)
                            throws CmsException {
                        return new CmsScriptTemplate(delegator, fields, assoc);
                    }
                }.findByIdAlways(getDelegator(), getScriptTemplateId(), false);
                this.scriptTemplate = scriptTemplate;
            }
            return scriptTemplate;
        }
        
        /**
         * Temporary clearing method during copy operations, SHOULD be
         * overridden by child classes to clear the other IDs.
         */
        protected abstract void clearTemplate();
        
        protected abstract void setTemplate(CmsDataObject template);
        
        protected abstract boolean hasTemplate();
        
        public Long getInputPosition() {
            Long inputPosition = entity.getLong("inputPosition");
            return inputPosition != null ? inputPosition : 0L;
        }

        public String getInvokeName() {
            return entity.getString("invokeName");
        }
        
        public static abstract class ScriptTemplateAssocWorker<T extends CmsScriptTemplateAssoc> extends DataObjectWorker<T> {
            protected ScriptTemplateAssocWorker(Class<T> dataObjectClass) {
                super(dataObjectClass);
            }
        }
    }
    
    public enum ScriptLang {
        // NOTE: Not all supported languages are listed here; just the ones we handle explicit
        GROOVY("groovy"),
        SCREEN_ACTIONS("screen-actions"),
        SIMPLE_METHOD("simple-method"),
        AUTO("auto"),
        NONE("none");
        
        private static final Set<String> nameSet;
        private static final Map<String, ScriptLang> nameMap;
        static {
            Map<String, ScriptLang> map = new HashMap<>();
            for(ScriptLang scriptLang : ScriptLang.values()) {
                map.put(scriptLang.getName(), scriptLang);
            }
            nameMap = map;
            nameSet = Collections.unmodifiableSet(new HashSet<>(map.keySet()));
        }
        
        private final String name;

        private ScriptLang(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

        public static ScriptLang fromName(String name) {
            return nameMap.get(name);
        }
        
        public static ScriptLang fromLocation(String fullLocation) throws CmsException {
            if (fullLocation != null) {
                String location = WidgetWorker.getScriptLocation(fullLocation);
                if (location.endsWith(".groovy")) {
                    return GROOVY;
                } else if (location.endsWith("Screens.xml")) { // WARN: this is a potentially problematic heuristic, but works with all known scipio files
                    return SCREEN_ACTIONS;
                } else if (location.endsWith(".xml")) {
                    return SIMPLE_METHOD;
                } else {
                    return AUTO;
                }
            }
            return null;
        }
        
        public static Set<String> getNames() {
            return nameSet;
        }
    }
    
    @SuppressWarnings("serial")
    public static abstract class ScriptExecutor implements Serializable {
        
        private static final DummyScriptExecutor dummyExecutor = new DummyScriptExecutor();
        
        protected final String id;
        protected final String invokeName;

        protected ScriptExecutor(String id, String invokeName) {
            this.id = id;
            this.invokeName = invokeName;
        }
        
        // Factory methods
        
        /**
         * NOTE: Explicit script type always has priority. If null, can usually be inferred from location,
         * but currently cannot infer from body alone.
         */
        public static ScriptExecutor getExecutor(String langStr, String id, TemplateBodySource tmplBodySrc, String invokeName) {
            // NOTE: here location has priority, contrary to some other code, because executing
            // from specified location is unambiguous and faster
            String bodyOrLocation = tmplBodySrc.getLocation();
            boolean isLocation = UtilValidate.isNotEmpty(bodyOrLocation);
            if (!isLocation) {
                bodyOrLocation = tmplBodySrc.getAvailableBody();
                if (bodyOrLocation == null) {
                    throw new CmsException("Script '" + id + "' has no location or body to execute");
                }
            }
            
            ScriptLang knownLang;
            if (UtilValidate.isNotEmpty(langStr)) {
                knownLang = ScriptLang.fromName(langStr);
            } else {
                knownLang = ScriptLang.AUTO;
                langStr = null;
            }

            // NOTE: if explLang==null, it means langStr is an unknown lang but still explicit
            if (knownLang == ScriptLang.AUTO) {
                if (isLocation) {
                    knownLang = ScriptLang.fromLocation(tmplBodySrc.getLocation());
                    if (knownLang == null) {
                        // double fallback to AutoLocationScriptExecutor
                        knownLang = ScriptLang.AUTO;
                    }
                } else {
                    if (langStr == null) {
                        throw new CmsException("Unable to determine script type for script body - no explicit type and no location (cannot be inferred from body alone)");
                    }
                }
            }
            
            if (knownLang == ScriptLang.GROOVY) {
                if (isLocation) {
                    return new GroovyLocationScriptExecutor(id, bodyOrLocation, invokeName);
                } else {
                    return new GroovyBodyScriptExecutor(id, bodyOrLocation, invokeName);
                }
            } else if (knownLang == ScriptLang.SCREEN_ACTIONS) {
                if (isLocation) {
                    return new ScreenActionsLocationScriptExecutor(id, bodyOrLocation, invokeName);
                } else {
                    throw new CmsException("Screen actions script cannot be run from stored body (script id: '" + id + "')");
                }
            } else if (knownLang == ScriptLang.SIMPLE_METHOD) {
                if (isLocation) {
                    return new SimpleMethodLocationScriptExecutor(id, bodyOrLocation, invokeName);
                } else {
                    throw new CmsException("Simple-method script cannot be run from stored body (script id: '" + id + "')");
                }
            } else if (knownLang == ScriptLang.NONE) {
                return dummyExecutor;
            } else {
                if (isLocation) {
                    return new AutoLocationScriptExecutor(id, bodyOrLocation, invokeName, langStr);
                } else {
                    return new AutoBodyScriptExecutor(id, bodyOrLocation, invokeName, langStr);
                }
            }
        }
        
        public static ScriptExecutor getExecutor(String id, TemplateBodySource tmplBodySrc, String invokeName) {
            return getExecutor(ScriptLang.AUTO.getName(), id, tmplBodySrc, invokeName);
        }
        
        public static ScriptExecutor getDummyExecutor() {
            return dummyExecutor;
        }
        
        
        // Public methods
        
        public abstract Object execute(Map<String, Object> context) throws Exception;
        
        public Object executeSafe(Map<String, Object> context) {
            try {
                return execute(context);
            } catch (Throwable t) {
                Debug.logError(t, "Cms: Error running script " + getScriptLogRepr() + ": " + t.getMessage() + getReqLogIdDelimStr(context), module);
                return null;
            }
        }
        
        public abstract ScriptLang getScriptLang();

        public abstract String getScriptLogRepr();

        
        // Internal implementation methods
        
        protected abstract Object run(Map<String, Object> context) throws Exception;
        
        protected String getReqLogIdDelimStr(Map<String, Object> context) {
            return CmsControlUtil.getReqLogIdDelimStr((HttpServletRequest) context.get("request"));
        }
        
        
        // Location and Body abstract classes
        
        public static abstract class LocationScriptExecutor extends ScriptExecutor {
            protected final String location;
            
            protected LocationScriptExecutor(String id, String location, String invokeName) {
                super(id, invokeName);
                this.location = location;
            }
            
            @Override
            public Object execute(Map<String, Object> context) throws Exception {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Running script from location: " + location + (invokeName != null ? "#" + invokeName : "") 
                            + (id != null ? " (id: " + id + ")" : "") + getReqLogIdDelimStr(context), module);
                }
                return run(context);
            }
            
            @Override
            public String getScriptLogRepr() {
                return "[id: " + id + ", location: " + location + (invokeName != null ? "#" + invokeName : "") + "]";
            }
        }
        
        public static abstract class BodyScriptExecutor extends ScriptExecutor {
            protected final String body;
 
            public BodyScriptExecutor(String id, String body, String invokeName) {
                super(id, invokeName);
                this.body = body;
            }

            @Override
            public Object execute(Map<String, Object> context) throws Exception {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Running script from body" + (id != null ? " (id: " + id + ")" : "") + getReqLogIdDelimStr(context), module);
                }
                return run(context);
            }
            
            @Override
            public String getScriptLogRepr() {
                return "[id: " + id + "]";
            }
        }
        

        // Language implementations
 
        public static class GroovyLocationScriptExecutor extends LocationScriptExecutor {
            protected GroovyLocationScriptExecutor(String id, String location, String invokeName) {
                super(id, location, invokeName);
            }

            @Override
            protected Object run(Map<String, Object> context) throws Exception {
                return GroovyUtil.runScriptAtLocation(location, invokeName, context);
            }
            
            @Override
            public ScriptLang getScriptLang() {
                return ScriptLang.GROOVY;
            }
        }
        
        public static class GroovyBodyScriptExecutor extends AutoBodyScriptExecutor {
     
            protected GroovyBodyScriptExecutor(String id, String body, String invokeName) {
                super(id, body, invokeName, "groovy");
            }
            
            @Override
            public ScriptLang getScriptLang() {
                return ScriptLang.GROOVY;
            }
        }
 
        
        public static class SimpleMethodLocationScriptExecutor extends LocationScriptExecutor {

            protected SimpleMethodLocationScriptExecutor(String id, String location, String invokeName) {
                super(id, location, invokeName);
            }

            @Override
            protected Object run(Map<String, Object> context) throws Exception {
                String method = invokeName;

                // Based on org.ofbiz.widget.model.AbstractModelAction.Script.runAction
                Map<String, Object> localContext = new HashMap<>();
                localContext.putAll(context);
                DispatchContext ctx = WidgetWorker.getDispatcher(context).getDispatchContext();
                MethodContext methodContext = new MethodContext(ctx, localContext, null);
                String result = SimpleMethod.runSimpleMethod(location, method, methodContext);
                context.putAll(methodContext.getResults());
                return result;
            }
            
            @Override
            public ScriptLang getScriptLang() {
                return ScriptLang.SIMPLE_METHOD;
            }
        }

        
        public static class ScreenActionsLocationScriptExecutor extends LocationScriptExecutor {

            protected ScreenActionsLocationScriptExecutor(String id, String location, String invokeName) {
                super(id, location, invokeName);
            }

            @Override
            protected Object run(Map<String, Object> context) throws Exception {
                ModelScreen widget = ScreenFactory.getScreenFromLocation(location, invokeName);
                AbstractModelAction.runSubActions(widget.getSection().getActions(), context); // NOTE: wraps in RunTimeExceptions
                return null;
            }
            
            @Override
            public ScriptLang getScriptLang() {
                return ScriptLang.SCREEN_ACTIONS;
            }
        }
        
        public static class AutoLocationScriptExecutor extends LocationScriptExecutor {
            // NOTE: we are simply ignoring the explicit lang here, ofbiz helpers won't let us override
            //protected final String lang; 
            
            protected AutoLocationScriptExecutor(String id, String location, String invokeName, String lang) {
                super(id, location, invokeName);
                // this.lang = lang;  // NOTE: ignoring for now
            }

            @Override
            protected Object run(Map<String, Object> context) throws Exception {
                return ScriptUtil.executeScript(location, invokeName, context);
            }

            @Override
            public ScriptLang getScriptLang() {
                return ScriptLang.AUTO;
            }
        }
        
        public static class AutoBodyScriptExecutor extends BodyScriptExecutor {
            protected final Scriptlet scriptlet;
            
            public AutoBodyScriptExecutor(String id, String body, String invokeName, String lang) {
                super(id, body, invokeName);
                this.scriptlet = new Scriptlet(lang + ":" + body);
            }
            
            @Override
            protected Object run(Map<String, Object> context) throws Exception {
                return scriptlet.executeScript(context);
            }

            @Override
            public ScriptLang getScriptLang() {
                return ScriptLang.AUTO;
            }
        }

        public static class DummyScriptExecutor extends ScriptExecutor {
            private DummyScriptExecutor() {
                super("NONE", null);
            }

            @Override
            public ScriptLang getScriptLang() {
                return ScriptLang.NONE;
            }

            @Override
            public Object execute(Map<String, Object> context) throws Exception {
                return null;
            }

            @Override
            public String getScriptLogRepr() {
                return "";
            }

            @Override
            protected Object run(Map<String, Object> context) throws Exception {
                return null;
            }
        }
    }
    
    
    @Override
    public ScriptTemplateWorker getWorkerInst() {
        return ScriptTemplateWorker.worker;
    }
    
    public static ScriptTemplateWorker getWorker() {
        return ScriptTemplateWorker.worker;
    }

    public static class ScriptTemplateWorker extends DataObjectWorker<CmsScriptTemplate> {
        private static final ScriptTemplateWorker worker = new ScriptTemplateWorker();
        
        protected ScriptTemplateWorker() {
            super(CmsScriptTemplate.class);
        }

        @Override
        public CmsScriptTemplate makeFromValue(GenericValue value) throws CmsException {
            return new CmsScriptTemplate(value);
        }

        @Override
        public CmsScriptTemplate makeFromFields(Delegator delegator, Map<String, ?> fields) throws CmsException {
            return new CmsScriptTemplate(delegator, fields);
        }
        
        @Override
        public CmsScriptTemplate findById(Delegator delegator, String id, boolean useCache) throws CmsException {
            return findById(delegator, id, useCache, null);
        }
        
        public CmsScriptTemplate findById(Delegator delegator, String id, boolean useCache, HttpServletRequest request) throws CmsException {
            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsScriptTemplate> cache = null;
            if (useGlobalCache) {
                cache = idCache;
            }
            
            String key = delegator.getDelegatorName() + "::" + id;
            CmsScriptTemplate script = null;
            CacheEntry<CmsScriptTemplate> scriptEntry = null;
            
            if (useGlobalCache) {
                scriptEntry = cache.getEntry(key);
            }

            if (scriptEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving script template from database: id: " + id + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                script = findOne(delegator, UtilMisc.toMap("scriptTemplateId", id), 
                        isUseDbCacheStatic(useCache));

                if (useGlobalCache) {
                    cache.put(key, script);
                }
            } else {
                if (scriptEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving script template from cache: id: " + id + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    script = scriptEntry.getValue();
                }
            }

            return script;
        }
        
        public CmsScriptTemplate findByName(Delegator delegator, String name, String webSiteId, boolean webSiteIdOptional, boolean useCache) throws CmsException {
            return findByName(delegator, name, webSiteId, webSiteIdOptional, useCache, null);
        }
        
        /**
         * Finds by name and optional webSiteId. 
         * NOTE: if no webSiteId passed, it preferentially returns the records having no webSiteId.
         * NOTE: 2017-03-24: webSiteId IS CURRENTLY IGNORED.
         */
        public CmsScriptTemplate findByName(Delegator delegator, String name, String webSiteId, boolean webSiteIdOptional, boolean useCache, HttpServletRequest request) throws CmsException {
            boolean useGlobalCache = isUseGlobalObjCacheStatic(useCache);
            CmsObjectCache<CmsScriptTemplate> cache = null;
            if (useGlobalCache) {
                cache = nameCache;
            }
            if (webSiteId != null && webSiteId.isEmpty()) {
                webSiteId = null;
            }
            String key = delegator.getDelegatorName() + "::" + name + "::" + (webSiteId != null ? webSiteId : (webSiteIdOptional ? "_OPT_" : ""));
            CmsScriptTemplate script = null;
            CacheEntry<CmsScriptTemplate> scriptEntry = null;
            
            if (useGlobalCache) {
                scriptEntry = cache.getEntry(key);
            }

            if (scriptEntry == null) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Retrieving script template from database: name: " + name + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                Map<String, Object> fields = UtilMisc.toMap("templateName", name);
//                if (!webSiteIdOptional || webSiteId != null) {
//                    fields.put("webSiteId", webSiteId);
//                }
                // NOTE: always null webSiteIds first - this matters
//                List<CmsScriptTemplate> scripts = findAll(delegator, fields, UtilMisc.toList("webSiteId"), isUseDbCacheStatic(useCache));
                List<CmsScriptTemplate> scripts = findAll(delegator, fields, null, isUseDbCacheStatic(useCache));
                if (scripts.size() > 0) {
                    script = scripts.get(0);
                }
                if (scripts.size() > 1) {
//                    if (!webSiteIdOptional || webSiteId != null) {
                    Debug.logError("Cms: Multiple script templates with name '" + name + "' and webSiteId '" + webSiteId + "' found; using first found (id: " + script.getId() + ")", module);
//                    } else if (asset.getWebSiteId() != null) {
//                        // if lookup by name only, it's usually because we expected only one result,
//                        // either one with webSiteId null (no log warning) or only one webSiteId
//                        Debug.logWarning("Cms: Multiple asset templates with name '" + name + "' and having a webSiteId found; using first found (id: " + asset.getId() + ", webSiteId: " + asset.getWebSiteId() + ")", module);
//                    }
                }
                
                if (useGlobalCache) {
                    cache.put(key, script);
                }
            } else {
                if (scriptEntry.hasValue()) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logVerbose("Cms: Retrieving script template from cache: name: " + name + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    script = scriptEntry.getValue();
                }
            }

            return script;
        }
    }
    
    @Override
    public void acceptEntityDepsVisitor(CmsEntityVisitor visitor, GenericValue relValue, VisitRelation relValueRelation, CmsMajorObject majorDataObj) throws Exception {
        CmsEntityVisit.acceptRelatedEntityDepsVisitor(visitor, VisitRelPlan.visitRelations, this.getEntity(), relValueRelation, relValue, this);
    }
    
    public static class VisitRelPlan extends VisitRelations.BuildPlan {
        public static final VisitRelPlan INSTANCE = new VisitRelPlan("CmsScriptTemplate");
        static final VisitRelations visitRelations = INSTANCE.buildSafe();
        public VisitRelPlan(String majorEntityName) { super(majorEntityName); }
        @Override public VisitRelations.Builder planDefinition(Delegator delegator) throws Exception {
            return newBuilder(delegator)
                .entity("CmsScriptTemplate")
                    .self();
        }
    }
    
}
