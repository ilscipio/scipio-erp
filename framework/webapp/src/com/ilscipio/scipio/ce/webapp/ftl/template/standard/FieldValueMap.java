package com.ilscipio.scipio.ce.webapp.ftl.template.standard;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.UtilGenerics;

import com.ilscipio.scipio.ce.webapp.ftl.template.standard.FieldValueMap.FullParamsFieldValueMap.FullParamsFieldInfo;
import com.ilscipio.scipio.ce.webapp.ftl.template.standard.FieldValueMap.FullParamsFieldValueMap.FullParamsFieldSources;

/**
 * SCIPIO: Generalized template field value map and auto-value logic
 * used to determine the appropriate initial values to use for a form.
 * <p>
 * This especially for use with - but not restricted to - Scipio's
 * standard templating macro <code>@field</code>.
 * <p>
 * TODO: NOT IMPLEMENTED - THIS IS A SKELETON AND BRAINSTORM ONLY UNTIL
 * FURTHER NOTICE.
 */
public abstract class FieldValueMap <F extends FieldValueMap.FieldInfo, S extends FieldValueMap.FieldSources> implements Map<String, Object> {

    /**
     * Resolved, cached field values.
     */
    protected final Map<String, Object> resolved;
    
    protected boolean allResolved = false;
    
    protected FieldValueMap() {
        // TODO Auto-generated constructor stub
        this.resolved = new HashMap<>();
    }


    /**
     * Gets an auto-value map of given type (full specification). 
     * <p>
     * <pre>
     * * {{{params}}}: looks for value in overrides map, then parameters map, then defaults map
     * * {{{record}}}: looks for value in overrides map, then record map, then defaults map
     * * {{{defaults-only}}}: looks for value in overrides map, then defaults map
     * * {{{params-record}}}: looks for value in overrides map, then parameters map, then record, then defaults map.
     *   This may be
     * * {{{params-or-record}}}: looks for value in overrides map, then EITHER parameters map OR record map, then defaults map
     *   At current time (2016-07-08), the selection of parameters or record map default behavior is based on whether an event
     *   error occurred ({{{isError}}} boolean context field).
     * * {{{standard}}}: In scipio standard API, currently (2016-07-08), this is the same params-or-record, currently considered the standard behavior.
     * </pre>
     */
    public static FieldValueMap<?, ?> getAutoValueMap(String type, SubmitConfig submitConfig, 
            FieldSources fieldSources, Map<String, ?> fieldInfoMap) {
        Map<String, FullParamsFieldInfo> targetFieldInfoMap = new HashMap<>();
        for(Map.Entry<String, ?> entry : fieldInfoMap.entrySet()) {
            Object fieldInfo = entry.getValue();
            if (fieldInfo instanceof FullParamsFieldInfo) {
                targetFieldInfoMap.put(entry.getKey(), (FullParamsFieldInfo) fieldInfo);
            } else if (fieldInfo instanceof Map) {
                targetFieldInfoMap.put(entry.getKey(), new FullParamsFieldInfo(UtilGenerics.<String, Object> checkMap(entry.getValue())));
            } else {
                throw new IllegalArgumentException("Invalid FieldInfo type (must be map or FieldInfo subtype)");
            }
        }
        return new FullParamsFieldValueMap(submitConfig, (FullParamsFieldSources) fieldSources, targetFieldInfoMap);
    }
    
    public static FieldValueMap<?, ?> getAutoValueMap(Map<String, ?> args) {
        String type = (String) args.get("type");
        // FIXME: FieldSources subclass depends on type
        return getAutoValueMap(type, new SubmitConfig(args), new FullParamsFieldSources(args), 
                UtilGenerics.<String, Object> checkMap(args.get("fieldInfoMap")));
    }
    
    
    /**
     * Determines the initial value that the field with the given name should
     * have in a form.
     */
    public Object getAutoValue(String name) {
        return getAutoValue(name, getFieldInfoMap().get(name));
    }
    
    /**
     * Determines the initial value that the field with the given name should
     * have in a form, with fieldInfo
     * specified at time of fetch instead of initialization.
     */
    public abstract Object getAutoValue(String name, F fieldInfo);
    
    /**
     * Determines the initial value that the field with the given name should
     * have in a form, with fieldInfo
     * specified at time of fetch instead of initialization.
     */
    public Object getAutoValue(String name, Map<String, ?> fieldInfo) {
        return getAutoValue(name, getFieldInfo(fieldInfo));
    }
    
    protected abstract F getFieldInfo(Map<String, ?> fields);
    
    protected abstract Map<String, F> getFieldInfoMap();

    
    protected abstract S getFieldSources();
    
    /**
     * Resolves all initial values for all fields names in form.
     */
    public Map<String, Object> getAllAutoValues() {
        Map<String, Object> autoValues = new HashMap<>();
        for(String fieldName : getAllFieldNames()) {
            autoValues.put(fieldName, getAutoValue(fieldName));
        }
        return autoValues;
    }

    /**
     * BEST-EFFORT method to return all field names. In some cases,
     * this cannot be known in advance, which will limit which methods
     * on this class can be used.
     */
    public abstract Set<String> getAllFieldNames();
    
    /**
     * Marks all fields resolved or not resolved. Can be used
     * to bypass class's default resolving behavior by client code.
     */
    public void markAllResolved(boolean allResolved) {
        this.allResolved = allResolved;
    }
    
    /**
     * Resolves the values for any fields not yet queried.
     */
    public void resolveAll() {
        for(String fieldName : getAllFieldNames()) {
            if (!resolved.containsKey(fieldName)) {
                resolved.put(fieldName, getAutoValue(fieldName));
            }
        }
        this.allResolved = true;
    }
    
    protected Map<String, Object> getAllResolved() {
        if (!allResolved) {
            resolveAll();
        }
        return resolved;
    }
    
    /**
     * Returns number of fields.
     */
    @Override
    public int size() {
        return getAllFieldNames().size();
    }
    
    /**
     * Returns true if and only if contains no fields.
     */
    @Override
    public boolean isEmpty() {
        return getAllFieldNames().isEmpty();
    }

    /**
     * Checks if contains the field with given name.
     */
    @Override
    public boolean containsKey(Object key) {
        return getAllFieldNames().contains(key);
    }

    /**
     * Checks if the given value is part of resolved values.
     * NOTE: This forces resolution of all field values.
     */
    @Override
    public boolean containsValue(Object value) {
        return getAllResolved().containsValue(value);
    }

    /**
     * Gets the resolved value for the given field name.
     * The value is cached and retrieved from cache when possible.
     */
    @Override
    public Object get(Object key) {
        if (resolved.containsKey(key)) {
            return resolved.get(key);
        } else {
            String name = (key != null) ? key.toString() : null;
            Object value;
            if (name != null && name.length() > 0) {
                value = getAutoValue(name);
            } else {
                value = null;
            }
            resolved.put(name, value);
            return value;
        }
    }
    
    /**
     * Gets the resolved value for the given field name, with fieldInfo
     * specified at time of fetch instead of initialization.
     * The value is cached and retrieved from cache when possible.
     */
    public Object get(Object key, F fieldInfo) {
        return (key != null) ? getAutoValue(key.toString(), fieldInfo) : null;
    }

    /**
     * Gets the resolved value for the given field name, with fieldInfo
     * specified at time of fetch instead of initialization.
     * The value is cached and retrieved from cache when possible.
     */
    public Object get(Object key, Map<String, ?> fieldInfo) {
        return (key != null) ? getAutoValue(key.toString(), fieldInfo) : null;
    }
    
    /**
     * Overrides the resolved value for a field name with a custom value.
     * <p>
     * NOTE: This bypasses any auto value logic determined by this or during
     * construction.
     */
    @Override
    public Object put(String key, Object value) {
        return resolved.put(key, value);
    }

    /**
     * Clears the cached resolved value for the given field name.
     */
    @Override
    public Object remove(Object key) {
        return resolved.remove(key);
    }

    /**
     * Overrides the resolved values with user-supplied values.
     * <p>
     * NOTE: This bypasses any auto value logic determined by this or during
     * construction.
     */
    @Override
    public void putAll(Map<? extends String, ? extends Object> m) {
        resolved.putAll(m);
    }
    
    /**
     * Clears all cached resolved field values.
     */
    @Override
    public void clear() {
        resolved.clear();
        allResolved = false;
    }
    
    /**
     * Iterates all field names.
     */
    @Override
    public Set<String> keySet() {
        return getAllFieldNames();
    }
    
    /**
     * Iterates all field values.
     * NOTE: forces resolution of all fields.
     */
    @Override
    public Collection<Object> values() {
        return getAllResolved().values();
    }

    /**
     * Iterates all field entries.
     * NOTE: forces resolution of all fields.
     */
    @Override
    public Set<java.util.Map.Entry<String, Object>> entrySet() {
        return getAllResolved().entrySet();
    }

    public static class SubmitConfig {
        /*
            submitDetectMethod      = (default|flag|post|none, default: default) Submit detection method
                                      * {{{default}}}: use {{{flag}}} if {{{submitFlagParam}}}, otherwise use {{{post}}}
                                      * {{{flag}}}: use {{{submitFlagParam}}}
                                      * {{{post}}}: use POST request check
                                      * {{{none}}}: none
            submitFlagParam         = Optional name of a parameter from params map whose presence determines if a form was submitted or not.
                                      Automatically implies {{{submitDetectMethod="flag"}}}.
                                      This parameter is checked using {{{??}}} operator, by simple presence.
                                      This is needed for parameters whose HTML inputs don't always submit a value.
                                      If a submit happened and the field is missing, then the field is given the value specified in {{{submitDefaultParamValue}}}.
                                      By default, submission is detected using presence of POST request, but in most cases,
                                      it is better to have submitFlagParam specified, easiest using @fields.
            submitDefaultParamValue = ((string), default: ""/[]/{}) Default param value to use if submitFlagParam checks out
            submitError             = ((boolean)|"", default: "") Explicit success/error flag
                                      If not specified as boolean (empty string), uses isError context variable.
                              
         */
        
        protected final String submitDetectMethod;
        protected final String submitFlagParam;
        protected final String submitDefaultParamValue;
        protected final String submitError;
        
        public SubmitConfig(String submitDetectMethod, String submitFlagParam, String submitDefaultParamValue,
                String submitError) {
            this.submitDetectMethod = submitDetectMethod;
            this.submitFlagParam = submitFlagParam;
            this.submitDefaultParamValue = submitDefaultParamValue;
            this.submitError = submitError;
        }
        
        public SubmitConfig(Map<String, ?> fields) {
            this.submitDetectMethod = (String) fields.get("submitDetectMethod");
            this.submitFlagParam = (String) fields.get("submitFlagParam");
            this.submitDefaultParamValue = (String) fields.get("submitDefaultParamValue");
            this.submitError = (String) fields.get("submitError");
        }
        
        public String getSubmitDetectMethod() {
            return submitDetectMethod;
        }
        
        public String getSubmitFlagParam() {
            return submitFlagParam;
        }
        
        public String getSubmitDefaultParamValue() {
            return submitDefaultParamValue;
        }
        
        public String getSubmitError() {
            return submitError;
        }
    }
    
    public abstract static class FieldInfo {
        protected FieldInfo() {
        }
        
        protected FieldInfo(Map<String, ?> fields) {
        }
    }
    
    
    public abstract static class FieldSources {
        protected FieldSources() {
        }
        
        protected FieldSources(Map<String, ?> fields) {
        }
    }
    

    public static class FullParamsFieldValueMap extends FieldValueMap<FullParamsFieldValueMap.FullParamsFieldInfo, FullParamsFieldValueMap.FullParamsFieldSources> {

        protected final SubmitConfig submitConfig;
        protected final FullParamsFieldSources fieldSources;
        protected final Map<String, FullParamsFieldInfo> fieldInfoMap;

        public FullParamsFieldValueMap(SubmitConfig submitConfig, FullParamsFieldSources fieldSources, Map<String, FullParamsFieldInfo> fieldInfoMap) {
            super();
            this.submitConfig = submitConfig;
            this.fieldInfoMap = fieldInfoMap != null ? fieldInfoMap : Collections.<String, FullParamsFieldInfo> emptyMap();
            this.fieldSources = fieldSources;
        }
        
        @Override
        public Object getAutoValue(String name, FullParamsFieldInfo fieldInfo) {
            // TODO Auto-generated method stub

            return null;
        }
        
        @Override
        protected Map<String, FullParamsFieldInfo> getFieldInfoMap() {
            return fieldInfoMap;
        }

        @Override
        protected FullParamsFieldSources getFieldSources() {
            return fieldSources;
        }
        
        @Override
        protected FullParamsFieldInfo getFieldInfo(Map<String, ?> fields) {
            return new FullParamsFieldInfo(fields);
        }
        

        @Override
        public Set<String> getAllFieldNames() {
            if (fieldSources.recordMap != null) {
                // Normally recordMap will be used here, but always support fieldInfoMap for this
                if (fieldSources.recordMap.size() > fieldInfoMap.size()) {
                    return fieldSources.recordMap.keySet();
                } else {
                    return fieldInfoMap.keySet();
                }
            } else {
                // WARN: This relies on client specifying fieldInfoMap,
                // otherwise resolved is nothing more than best-effort
                if (resolved.size() > fieldInfoMap.size()) {
                    return resolved.keySet();
                } else {
                    return fieldInfoMap.keySet();
                }
            }
        }

        public static class FullParamsFieldInfo extends FieldInfo {
            protected final String overrideName;
            protected final String paramName;
            protected final String recordName;
            protected final String defaultName;
            protected final String suffix;
            
            public FullParamsFieldInfo(String overrideName, String paramName, String recordName, String defaultName,
                    String suffix) {
                this.overrideName = overrideName;
                this.paramName = paramName;
                this.recordName = recordName;
                this.defaultName = defaultName;
                this.suffix = suffix;
            }
            
            public FullParamsFieldInfo(Map<String, ?> fields) {
                super(fields);
                this.overrideName = (String) fields.get("overrideName");
                this.paramName = (String) fields.get("paramName");
                this.recordName = (String) fields.get("recordName");
                this.defaultName = (String) fields.get("defaultName");
                this.suffix = (String) fields.get("suffix");
            }

            public String getOverrideName() {
                return overrideName;
            }

            public String getParamName() {
                return paramName;
            }

            public String getRecordName() {
                return recordName;
            }

            public String getDefaultName() {
                return defaultName;
            }

            public String getSuffix() {
                return suffix;
            }
        }
        
        public static class FullParamsFieldSources extends FieldSources {
            protected final Map<String, ?> overridesMap;
            protected final Map<String, ?> paramsMap;        
            protected final Map<String, ?> recordMap;
            protected final Map<String, ?> defaultsMap;
            
            public FullParamsFieldSources(Map<String, ?> overridesMap, Map<String, ?> paramsMap,
                    Map<String, ?> recordMap, Map<String, ?> defaultsMap) {
                super();
                this.overridesMap = overridesMap;
                this.paramsMap = paramsMap;
                this.recordMap = recordMap;
                this.defaultsMap = defaultsMap;
            }
            
            public FullParamsFieldSources(Map<String, ?> fields) {
                super(fields);
                this.overridesMap = UtilGenerics.checkMap(fields.get("overridesMap"));
                this.paramsMap = UtilGenerics.checkMap(fields.get("paramsMap"));
                this.recordMap = UtilGenerics.checkMap(fields.get("recordMap"));
                this.defaultsMap = UtilGenerics.checkMap(fields.get("defaultsMap"));
            }

            public Map<String, ?> getOverridesMap() {
                return overridesMap;
            }

            public Map<String, ?> getParamsMap() {
                return paramsMap;
            }

            public Map<String, ?> getRecordMap() {
                return recordMap;
            }

            public Map<String, ?> getDefaultsMap() {
                return defaultsMap;
            }
        }

    }
    
}
