package com.ilscipio.cato.webapp.ftl.lang;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.UtilGenerics;

import freemarker.core.Environment;
import freemarker.ext.beans.BeanModel;
import freemarker.ext.beans.BeansWrapper;
import freemarker.ext.beans.SimpleMapModel;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.DefaultArrayAdapter;
import freemarker.template.DefaultListAdapter;
import freemarker.template.ObjectWrapper;
import freemarker.template.ObjectWrapperAndUnwrapper;
import freemarker.template.SimpleHash;
import freemarker.template.SimpleSequence;
import freemarker.template.TemplateCollectionModel;
import freemarker.template.TemplateDirectiveModel;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateModelIterator;
import freemarker.template.TemplateScalarModel;
import freemarker.template.TemplateSequenceModel;
import freemarker.template.TemplateTransformModel;
import freemarker.template.utility.DeepUnwrap;
import freemarker.template.utility.RichObjectWrapper;

/**
 * Cato: Freemarker language utils.
 * <p>
 * These should generally not include Ofbiz-specific utils, except in the case
 * where the Ofbiz-specific code is merely a configuration of Freemarker (e.g.
 * selected usage of <code>BeansWrapper</code>).
 *
 * @see com.ilscipio.cato.webapp.ftl.CommonFtlUtil
 */
public abstract class LangFtlUtil {

    public static final String module = LangFtlUtil.class.getName();
    
    /**
     * Used for TemplateModel <-> unwrapped/raw value conversions.
     */
    public enum TemplateValueTargetType {
        PRESERVE,
        RAW,
        MODEL,
        SIMPLEMODEL,
        COMPLEXMODEL
    }

    public enum SetOperations {
        UNION,
        INTERSECT,
        DIFFERENCE
    }

    public interface FtlVarHandler {
        void setVariable(String name, TemplateModel model) throws TemplateModelException;
        TemplateModel getVariable(String name) throws TemplateModelException;
    }

    public static class GlobalFtlVarHandler implements FtlVarHandler {
        private final Environment env;
    
        public GlobalFtlVarHandler(Environment env) {
            this.env = env;
        }
    
        @Override
        public void setVariable(String name, TemplateModel model) throws TemplateModelException {
            env.setGlobalVariable(name, model);
        }
    
        @Override
        public TemplateModel getVariable(String name) throws TemplateModelException {
            return env.getGlobalVariable(name);
        }
    }

    public static class CurrentFtlVarHandler implements FtlVarHandler {
        private final Environment env;
    
        public CurrentFtlVarHandler(Environment env) {
            this.env = env;
        }
    
        @Override
        public void setVariable(String name, TemplateModel model) throws TemplateModelException {
            env.setVariable(name, model);
        }
    
        @Override
        public TemplateModel getVariable(String name) throws TemplateModelException {
            return env.getVariable(name);
        }
    }

    public static class LocalFtlVarHandler implements FtlVarHandler {
        private final Environment env;
    
        public LocalFtlVarHandler(Environment env) {
            this.env = env;
        }
    
        @Override
        public void setVariable(String name, TemplateModel model) throws TemplateModelException {
            env.setLocalVariable(name, model);
        }
    
        @Override
        public TemplateModel getVariable(String name) throws TemplateModelException {
            return env.getLocalVariable(name);
        }
    }

    protected LangFtlUtil() {
    }

    /**
     * Unwraps template model; if cannot, returns null.
     */
    public static Object unwrapOrNull(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            Object res = DeepUnwrap.permissiveUnwrap(templateModel);
            if (res != templateModel) {
                return res;
            }
            else {
                return null;
            }
        }
        else {
            return null;
        }
    }

    /**
     * If TemplateModel, unwraps value, or if cannot, returns null;
     * if not TemplateModel, returns as-is.
     * <p>
     * Ensures no TemplateModels remain.
     */
    public static Object unwrapOrNull(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            Object res = DeepUnwrap.permissiveUnwrap((TemplateModel) value);
            if (res != value) {
                return res;
            }
            else {
                return null;
            }
        }
        else {
            return value;
        }
    }

    /**
     * Unwraps template model; if cannot, returns as-is.
     */
    public static Object unwrapPermissive(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            return DeepUnwrap.permissiveUnwrap(templateModel);
        }
        else {
            return null;
        }
    }

    /**
     * Unwraps value; if cannot, returns value, even if still TemplateModel.
     */
    public static Object unwrapPermissive(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            return DeepUnwrap.permissiveUnwrap((TemplateModel) value);
        }
        else {
            return value;
        }
    }

    /**
     * Unwraps template model; if cannot, throws exception. If null, returns null.
     */
    public static Object unwrap(TemplateModel templateModel) throws TemplateModelException {
        if (templateModel != null) {
            return DeepUnwrap.unwrap(templateModel); // will throw exception if improper type
        }
        else {
            return null;
        }
    }

    /**
     * If template model, unwraps, or if cannot, throws exception;
     * if not template model or null, returns value.
     */
    public static Object unwrap(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel) {
            return DeepUnwrap.unwrap((TemplateModel) value);
        }
        else {
            return value;
        }
    }

    /**
     * Unwraps template model; if cannot, throws exception.
     * <p>
     * Interpretation of null depends on the ObjectWrapper.
     */
    public static Object unwrapAlways(TemplateModel templateModel) throws TemplateModelException {
        // FIXME? should all these DeepUnwrap.unwrap calls be more like FreeMarkerWorker.getWrappedObject(templateModel) instead??
        return DeepUnwrap.unwrap(templateModel); // will throw exception if improper type
    }

    /**
     * Unwraps value if template model and unwrappable; else exception.
     * <p>
     * Interpretation of null depends on the ObjectWrapper.
     */
    public static Object unwrapAlways(Object value) throws TemplateModelException {
        if (value instanceof TemplateModel || value == null) {
            return DeepUnwrap.unwrap((TemplateModel) value);
        }
        else {
            throw new TemplateModelException("Cannot unwrap non-TemplateModel value (type " + value.getClass().getName() + ")");
        }
    }

    public static String camelCaseToDashLowerName(String name) {
        // TODO: optimize
        return name.replaceAll("([A-Z])", "-$1").toLowerCase();
    }

    public static <K, V> Map<K, V> concatMaps(Map<? extends K, ? extends V> first, Map<? extends K, ? extends V> second) {
        Map<K, V> res = new LinkedHashMap<K, V>();
        if (first != null) {
            res.putAll(first);
        }
        if (second != null) {
            res.putAll(second);
        }
        return res;
    }

    /**
     * Checks if the given model matches the logical FTL object type.
     * 
     * @see com.ilscipio.cato.webapp.ftl.lang.OfbizFtlObjectType
     */
    public static boolean isObjectType(String ftlTypeName, TemplateModel object) {
        return OfbizFtlObjectType.isObjectTypeSafe(ftlTypeName, object);
    }

    public static Object getMapKeys(TemplateModel object) throws TemplateModelException {
        if (OfbizFtlObjectType.COMPLEXMAP.isObjectType(object)) {
            // would be safer to let the wrapper do it, but we know it's just a BeanModel in Ofbiz so we can optimize.
            Map<Object, Object> wrappedObject = UtilGenerics.cast(((WrapperTemplateModel) object).getWrappedObject());
            return wrappedObject.keySet();
        }
        else if (object instanceof TemplateHashModelEx) {
            return ((TemplateHashModelEx) object).keys();
        }
        else {
            throw new TemplateModelException("object is not a map or does not support key iteration");
        }
    }

    /**
     * Shallow-copies map or list. Note: won't preserve order for maps.
     * 
     * @param object
     * @param toSimpleType if true, converts to simple FTL type instead of beans, where possible
     * @return
     * @throws TemplateModelException
     */
    public static Object copyObject(TemplateModel object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (OfbizFtlObjectType.COMPLEXMAP.isObjectType(object) || (object instanceof TemplateHashModelEx && OfbizFtlObjectType.MAP.isObjectType(object))) {
            return LangFtlUtil.copyMap(object, null, null, targetType, objectWrapper);
        }
        else if (object instanceof TemplateCollectionModel || object instanceof TemplateSequenceModel) {
            return LangFtlUtil.copyList(object, targetType, objectWrapper);
        }
        else {
            throw new TemplateModelException("object is not cloneable");
        }
    }

    public static Object copyMap(TemplateModel object, Set<String> inExKeys, Boolean include, 
            TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (OfbizFtlObjectType.COMPLEXMAP.isObjectType(object)) {
            // would be safer to let the wrapper do it, but we know it's just a BeanModel in Ofbiz so we can optimize.
            Map<String, Object> wrappedObject = UtilGenerics.cast(((WrapperTemplateModel) object).getWrappedObject());
            // TODO: this only handles most urgent targetType case
            if (targetType == TemplateValueTargetType.SIMPLEMODEL) {
                return LangFtlUtil.copyMapToSimple(wrappedObject, inExKeys, include, objectWrapper);
            }
            else {
                return LangFtlUtil.copyMapToRawMap(wrappedObject, inExKeys, include);
            }
        }
        else if (object instanceof TemplateHashModel && OfbizFtlObjectType.MAP.isObjectType(object)) {
            // TODO: this ignores targetType
            return LangFtlUtil.copyMapToSimple((TemplateHashModel) object, inExKeys, include, objectWrapper);
        }
        throw new TemplateModelException("Cannot copy map of type " + object.getClass().toString() + 
                " to target type: " + targetType.toString());        
    }

    public static SimpleHash copyMapToSimple(TemplateHashModel hashModel, Set<String> inExKeys, Boolean include, ObjectWrapper objectWrapper) throws TemplateModelException {
        SimpleHash res = new SimpleHash(objectWrapper);
        
        if (include == Boolean.TRUE) {
            if (inExKeys == null) {
                inExKeys = new HashSet<String>();
            }
            for(String key : inExKeys) {
                TemplateModel valueModel = hashModel.get(key);
                if (inExKeys.contains(key)) {
                    res.put(key, valueModel);
                }
            }                
        }
        else if (include == null || inExKeys == null || inExKeys.isEmpty()) {
            if (!(hashModel instanceof TemplateHashModelEx)) {
                throw new TemplateModelException("Hash to copy does not support ?keys");
            }
            
            TemplateCollectionModel keys = ((TemplateHashModelEx) hashModel).keys();
            TemplateModelIterator keysIt = keys.iterator();
    
            while(keysIt.hasNext()) {
                String key = ((TemplateScalarModel) keysIt.next()).getAsString();
                res.put(key, hashModel.get(key));
            }                
        }
        else {
            if (!(hashModel instanceof TemplateHashModelEx)) {
                throw new TemplateModelException("Hash to copy does not support ?keys");
            }
            
            TemplateCollectionModel keys = ((TemplateHashModelEx) hashModel).keys();
            TemplateModelIterator keysIt = keys.iterator();
    
            while(keysIt.hasNext()) {
                String key = ((TemplateScalarModel) keysIt.next()).getAsString();
                TemplateModel valueModel = hashModel.get(key);
                if (!inExKeys.contains(key)) {
                    res.put(key, valueModel);
                }
            } 
        }
        return res;
    }

    public static SimpleHash copyMapToSimple(Map<String, Object> map, Set<String> inExKeys, Boolean include, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (include == Boolean.TRUE) {
            SimpleHash res = new SimpleHash(objectWrapper);
            if (inExKeys == null) {
                inExKeys = new HashSet<String>();
            }
            for(String key : inExKeys) {
                Object valueModel = map.get(key);
                if (inExKeys.contains(key)) {
                    res.put(key, valueModel);
                }
            }     
            return res;
        }
        else if (include == null || inExKeys == null || inExKeys.isEmpty()) {
            return new SimpleHash(map, objectWrapper);          
        }
        else {
            SimpleHash res = new SimpleHash(objectWrapper);
            for(Map.Entry<String, Object> entry : map.entrySet()) {
                String key = entry.getKey();
                if (!inExKeys.contains(key)) {
                    res.put(key, entry.getValue());
                }                
            }
            return res;
        }
    }

    public static Map<String, Object> copyMapToRawMap(Map<String, Object> map, Set<String> inExKeys, Boolean include) throws TemplateModelException {
        if (include == Boolean.TRUE) {
            Map<String, Object> res = new HashMap<String, Object>(map.size());
            if (inExKeys == null) {
                inExKeys = new HashSet<String>();
            }
            for(String key : inExKeys) {
                Object valueModel = map.get(key);
                if (inExKeys.contains(key)) {
                    res.put(key, valueModel);
                }
            }     
            return res;
        }
        else if (include == null || inExKeys == null || inExKeys.isEmpty()) {
            return new HashMap<String, Object>(map);        
        }
        else {
            Map<String, Object> res = new HashMap<String, Object>(map.size());
            for(Map.Entry<String, Object> entry : map.entrySet()) {
                String key = entry.getKey();
                if (!inExKeys.contains(key)) {
                    res.put(key, entry.getValue());
                }                
            }
            return res;
        }
    }

    /**
     * Copies a list to a target model/raw list type. In general does not wrap/unwrap individual values.
     */
    public static Object copyList(Object object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (object instanceof Iterable) {
            return LangFtlUtil.copyList(UtilGenerics.<Iterable<Object>>cast(object), targetType, objectWrapper);
        }
        else if (object instanceof TemplateModel) {
            return LangFtlUtil.copyList((TemplateModel) object, targetType, objectWrapper);
        }
        throw new TemplateModelException("Cannot copy list of type " + object.getClass().toString() + 
            " to target type: " + targetType.toString());
    }

    public static Object copyList(Iterable<Object> object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (object instanceof Collection) {
            Collection<Object> collection = UtilGenerics.<Collection<Object>>cast(object);
            if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.RAW) {
                return new ArrayList<Object>(collection);
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                return new SimpleSequence(collection, objectWrapper);
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                return objectWrapper.wrap(new ArrayList<Object>(collection));
            }
        }
        else {
            Iterable<Object> iterable = UtilGenerics.<Iterable<Object>>cast(object);
            if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.RAW) {
                List<Object> res = new ArrayList<Object>();
                for(Object val : iterable) {
                    res.add(val);
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                SimpleSequence res = new SimpleSequence(objectWrapper);
                for(Object val : iterable) {
                    res.add(val);
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                List<Object> res = new ArrayList<Object>();
                for(Object val : iterable) {
                    res.add(val);
                }
                return objectWrapper.wrap(res);
            } 
        }
        throw new TemplateModelException("Cannot copy list of type " + object.getClass().toString() + 
                " to target type: " + targetType.toString());
    }

    public static Object copyList(TemplateModel object, TemplateValueTargetType targetType, ObjectWrapper objectWrapper) throws TemplateModelException {
        if (targetType == null) {
            targetType = TemplateValueTargetType.PRESERVE;
        }
        if (object instanceof TemplateCollectionModel) { // TODO: isObjectType
            TemplateCollectionModel collectionModel = (TemplateCollectionModel) object;
            if (targetType == TemplateValueTargetType.RAW) {
                List<Object> res = new ArrayList<Object>();
                TemplateModelIterator it = collectionModel.iterator();
                while(it.hasNext()) {
                    res.add(it.next());
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL || targetType == TemplateValueTargetType.PRESERVE) {
                return new SimpleSequence(collectionModel);
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                List<Object> res = new ArrayList<Object>();
                TemplateModelIterator it = collectionModel.iterator();
                while(it.hasNext()) {
                    res.add(it.next());
                }
                return objectWrapper.wrap(res);
            } 
        }
        else if (object instanceof TemplateSequenceModel) { // TODO: isObjectType
            TemplateSequenceModel seqModel = (TemplateSequenceModel) object;
            if (targetType == TemplateValueTargetType.RAW) {
                List<Object> res = new ArrayList<Object>();
                for(int i=0; i < seqModel.size(); i++) {
                    res.add(seqModel.get(i));
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL || targetType == TemplateValueTargetType.PRESERVE) {
                SimpleSequence res = new SimpleSequence(seqModel.size(), objectWrapper);
                for(int i=0; i < seqModel.size(); i++) {
                    res.add(seqModel.get(i));
                }
                return res;
            }
            else if (targetType == TemplateValueTargetType.COMPLEXMODEL) {
                List<Object> res = new ArrayList<Object>();
                for(int i=0; i < seqModel.size(); i++) {
                    res.add(seqModel.get(i));
                }
                return objectWrapper.wrap(res);
            } 
        }
        else if (object instanceof WrapperTemplateModel) {
            Object wrappedObj = ((WrapperTemplateModel) object).getWrappedObject();
            if (wrappedObj instanceof Collection) {
                Collection<Object> collection = UtilGenerics.<Collection<Object>>cast(object);
                if (targetType == TemplateValueTargetType.RAW) {
                    return new ArrayList<Object>(collection);
                }
                else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                    return new SimpleSequence(collection, objectWrapper);
                }
                else if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.COMPLEXMODEL) {
                    return objectWrapper.wrap(new ArrayList<Object>(collection));
                }
            }
            else if (wrappedObj instanceof Iterable) {
                Iterable<Object> iterable = UtilGenerics.<Iterable<Object>>cast(object);
                if (targetType == TemplateValueTargetType.RAW) {
                    List<Object> res = new ArrayList<Object>();
                    for(Object val : iterable) {
                        res.add(val);
                    }
                    return res;
                }
                else if (targetType == TemplateValueTargetType.MODEL || targetType == TemplateValueTargetType.SIMPLEMODEL) {
                    SimpleSequence res = new SimpleSequence(objectWrapper);
                    for(Object val : iterable) {
                        res.add(val);
                    }
                    return res;
                }
                else if (targetType == TemplateValueTargetType.PRESERVE || targetType == TemplateValueTargetType.COMPLEXMODEL) {
                    List<Object> res = new ArrayList<Object>();
                    for(Object val : iterable) {
                        res.add(val);
                    }
                    return objectWrapper.wrap(res);
                }
            }
        }
        throw new TemplateModelException("Cannot copy list of type " + object.getClass().toString() + 
                " to target type: " + targetType.toString());
    }

    public static TemplateHashModel toSimpleMap(ObjectWrapper objectWrapper, TemplateModel object) throws TemplateModelException {
        if (OfbizFtlObjectType.COMPLEXMAP.isObjectType(object)) {
            // would be safer to let the wrapper do it, but we know it's just a BeanModel in Ofbiz so we can optimize.
            Map<Object, Object> wrappedObject = UtilGenerics.cast(((WrapperTemplateModel) object).getWrappedObject());
            return new SimpleMapModel(wrappedObject, (BeansWrapper) objectWrapper);
        }
        else if (object instanceof TemplateHashModel) {
            return (TemplateHashModel) object;
        }
        else {
            throw new TemplateModelException("object is not a recognized map type");
        }
    }

    /**
     * Supposed to convert to simple sequence.
     * <p>
     * DEV NOTE: I stopped writing/testing this when found out most of the problems w.r.t. collections are not
     * the FTL types this time but the way they're used in Ofbiz templates.
     * FTL's CollectionModel (subclass of TemplateCollectionModel) is supposed to cover everything and
     * won't suffer from the same problems maps have.
     */
    @SuppressWarnings("unchecked")
    private static TemplateSequenceModel toSimpleSequence(ObjectWrapper objectWrapper, TemplateModel object) throws TemplateModelException {
        if (object instanceof TemplateSequenceModel) {
            return (TemplateSequenceModel) object;
        }
        else if (object instanceof WrapperTemplateModel) {
            WrapperTemplateModel wrapperModel = (WrapperTemplateModel) object;
            Object wrappedObject = wrapperModel.getWrappedObject();
            if (wrappedObject instanceof List) {
                return DefaultListAdapter.adapt((List<Object>) wrappedObject, (RichObjectWrapper) objectWrapper);
            }
            else if (wrappedObject instanceof Object[]) {
                return DefaultArrayAdapter.adapt((Object[]) wrappedObject, (ObjectWrapperAndUnwrapper) objectWrapper);
            }
            else if (wrappedObject instanceof Set) {
                throw new UnsupportedOperationException("Not yet implemented");
            }
            else if (wrappedObject instanceof Collection) {
                throw new UnsupportedOperationException("Not yet implemented");
            }
            else if (wrappedObject instanceof Iterable) {
                throw new UnsupportedOperationException("Not yet implemented");
            }
            else {
                throw new TemplateModelException("Cannot convert bean-wrapped object of type " + (object != null ? object.getClass() : "null") + " to simple sequence"); 
            }
        }
        else if (object instanceof TemplateCollectionModel) {
            TemplateCollectionModel collModel = (TemplateCollectionModel) object;
            SimpleSequence res = new SimpleSequence(objectWrapper);
            TemplateModelIterator it = collModel.iterator();
            while(it.hasNext()) {
                res.add(it.next());
            }
            return res;
        }
        else {
            throw new TemplateModelException("Cannot convert object of type " + (object != null ? object.getClass() : "null") + " to simple sequence"); 
        }
    }

    /**
     * Same as Freemarker's ?is_directive.
     * <p>
     * <em>NOTE:</em> This <em>must</em> have the exact same behavior as Freemarker's ?is_directive.
     * Please refer to Freemarker source code. 
     * Unfortunately there is no evident way of reusing their code from here...
     * <p>
     * <strong>WARNING:</strong> FIXME: This currently refers to the FTL freemarker.core.Macro class, which is set
     * to change at any time. this needs a better solution!!!
     */
    public static boolean isDirective(Object object) {
        return (object instanceof TemplateTransformModel || object instanceof freemarker.core.Macro || object instanceof TemplateDirectiveModel);
    }

    /**
     * Adds to simple hash from source map.
     * <p>
     * TODO: WARN: This is not currently BeanModel-aware (complex map).
     */    
    public static void addToSimpleMap(SimpleHash dest, TemplateHashModelEx source) throws TemplateModelException {
        TemplateCollectionModel keysModel = source.keys();
        TemplateModelIterator modelIt = keysModel.iterator();
        while(modelIt.hasNext()) {
            String key = ((TemplateScalarModel) modelIt.next()).getAsString();
            dest.put(key, source.get(key));
        }
    }

    public static void addToSimpleMap(SimpleHash dest, TemplateHashModel source, Set<String> keys) throws TemplateModelException {
        for(String key : keys) {
            dest.put(key, source.get(key));
        }
    }

    /**
     * Makes a simple hash from source map; only specified keys.
     * <p>
     * TODO: WARN: This is not currently BeanModel-aware (complex map).
     */
    public static SimpleHash makeSimpleMap(TemplateHashModel map, Set<String> keys, ObjectWrapper objectWrapper) throws TemplateModelException {
        SimpleHash res = new SimpleHash(objectWrapper);
        addToSimpleMap(res, map, keys);
        return res;
    }

    public static SimpleHash makeSimpleMap(TemplateHashModel map, TemplateCollectionModel keys, ObjectWrapper objectWrapper) throws TemplateModelException {
        SimpleHash res = new SimpleHash(objectWrapper);
        addToSimpleMap(res, map, LangFtlUtil.toStringSet(keys));
        return res;
    }

    public static SimpleHash makeSimpleMap(TemplateHashModel map, TemplateSequenceModel keys, ObjectWrapper objectWrapper) throws TemplateModelException {
        SimpleHash res = new SimpleHash(objectWrapper);
        addToSimpleMap(res, map, LangFtlUtil.toStringSet(keys));
        return res;
    }

    public static Set<String> toStringSet(TemplateCollectionModel collModel) throws TemplateModelException {
        Set<String> set = new HashSet<String>();
        TemplateModelIterator modelIt = collModel.iterator();
        while(modelIt.hasNext()) {
            set.add(((TemplateScalarModel) modelIt.next()).getAsString());
        }
        return set;
    }

    public static void addToStringSet(Set<String> dest, TemplateCollectionModel collModel) throws TemplateModelException {
        TemplateModelIterator modelIt = collModel.iterator();
        while(modelIt.hasNext()) {
            dest.add(((TemplateScalarModel) modelIt.next()).getAsString());
        }
    }

    public static Set<String> toStringSet(TemplateSequenceModel seqModel) throws TemplateModelException {
        Set<String> set = new HashSet<String>();
        for(int i=0; i < seqModel.size(); i++) {
            set.add(((TemplateScalarModel) seqModel.get(i)).getAsString());
        }
        return set;
    }

    public static void addToStringSet(Set<String> dest, TemplateSequenceModel seqModel) throws TemplateModelException {
        for(int i=0; i < seqModel.size(); i++) {
            dest.add(((TemplateScalarModel) seqModel.get(i)).getAsString());
        }
    }

    /**
     * Combines two maps with the given operator into a new hash.
     */
    public static TemplateHashModelEx combineMaps(TemplateHashModelEx first, TemplateHashModelEx second, SetOperations ops, 
            ObjectWrapper objectWrapper) throws TemplateModelException {
        SimpleHash res = new SimpleHash(objectWrapper);
        if (ops == null || ops == SetOperations.UNION) {
            // this is less efficient than freemarker + operator, but provides the "alternative" implementation, so have choice
            addToSimpleMap(res, first);
            addToSimpleMap(res, second);
        }
        else if (ops == SetOperations.INTERSECT) {
            Set<String> intersectKeys = toStringSet(second.keys());
            intersectKeys.retainAll(toStringSet(first.keys()));
            addToSimpleMap(res, second, intersectKeys);
        }
        else if (ops == SetOperations.DIFFERENCE) {
            Set<String> diffKeys = toStringSet(first.keys());
            diffKeys.removeAll(toStringSet(second.keys()));
            addToSimpleMap(res, first, diffKeys);
        }
        else {
            throw new TemplateModelException("Unsupported combineMaps operation");
        }
        return res;
    }

    public static Set<String> getAsStringSet(TemplateModel model) throws TemplateModelException {
        Set<String> exKeys = null;
        if (model != null) {
            if (model instanceof BeanModel && ((BeanModel) model).getWrappedObject() instanceof Set) {
                exKeys = UtilGenerics.cast(((BeanModel) model).getWrappedObject());
            }
            else if (model instanceof TemplateCollectionModel) {
                exKeys = new HashSet<String>();
                TemplateModelIterator keysIt = ((TemplateCollectionModel) model).iterator();
                while(keysIt.hasNext()) {
                    exKeys.add(((TemplateScalarModel) keysIt.next()).getAsString());
                }
            }
            else if (model instanceof TemplateSequenceModel) {
                TemplateSequenceModel seqModel = (TemplateSequenceModel) model;
                exKeys = new HashSet<String>(seqModel.size());
                for(int i=0; i < seqModel.size(); i++) {
                    exKeys.add(((TemplateScalarModel) seqModel.get(i)).getAsString());
                }
            }
            else {
                throw new TemplateModelException("Include/exclude keys argument not a collection or set of strings");
            }
        }
        return exKeys;
    }

    /**
     * Puts all values in hash into FTL variables, decided by a varHandler.
     * <p>
     * TODO: replace tests with a filter class similar to FtlVarHandler.
     * <p>
     * @see copyMapToSimple
     */
    public static void varsPutAll(TemplateHashModel hashModel, Set<String> inExKeys, Boolean include, Boolean onlyDirectives, 
            FtlVarHandler varHandler, Environment env) throws TemplateModelException {
        if (include == Boolean.TRUE) {
            if (inExKeys == null) {
                inExKeys = new HashSet<String>();
            }
            if (onlyDirectives == Boolean.TRUE) {
                for(String key : inExKeys) {
                    TemplateModel valueModel = hashModel.get(key);
                    if (isDirective(valueModel)) {
                        varHandler.setVariable(key, valueModel);
                    }
                }
            }
            else {
                for(String key : inExKeys) {
                    TemplateModel valueModel = hashModel.get(key);
                    if (inExKeys.contains(key)) {
                        varHandler.setVariable(key, valueModel);
                    }
                }                
            }
        }
        else if (include == null || inExKeys == null || inExKeys.isEmpty()) {
            if (!(hashModel instanceof TemplateHashModelEx)) {
                throw new TemplateModelException("Hash to copy does not support ?keys");
            }
            
            TemplateCollectionModel keys = ((TemplateHashModelEx) hashModel).keys();
            TemplateModelIterator keysIt = keys.iterator();
            if (onlyDirectives == Boolean.TRUE) {
                while(keysIt.hasNext()) {
                    String key = ((TemplateScalarModel) keysIt.next()).getAsString();
                    TemplateModel valueModel = hashModel.get(key);
                    if (isDirective(valueModel)) {
                        varHandler.setVariable(key, valueModel);
                    }
                }
            }
            else {
                while(keysIt.hasNext()) {
                    String key = ((TemplateScalarModel) keysIt.next()).getAsString();
                    varHandler.setVariable(key, hashModel.get(key));
                }                
            }
        }
        else {
            if (!(hashModel instanceof TemplateHashModelEx)) {
                throw new TemplateModelException("Hash to copy does not support ?keys");
            }
            
            TemplateCollectionModel keys = ((TemplateHashModelEx) hashModel).keys();
            TemplateModelIterator keysIt = keys.iterator();
            if (onlyDirectives == Boolean.TRUE) {
                while(keysIt.hasNext()) {
                    String key = ((TemplateScalarModel) keysIt.next()).getAsString();
                    TemplateModel valueModel = hashModel.get(key);
                    if (!inExKeys.contains(key) && isDirective(valueModel)) {
                        varHandler.setVariable(key, valueModel);
                    }
                }
            }
            else {
                while(keysIt.hasNext()) {
                    String key = ((TemplateScalarModel) keysIt.next()).getAsString();
                    TemplateModel valueModel = hashModel.get(key);
                    if (!inExKeys.contains(key)) {
                        varHandler.setVariable(key, valueModel);
                    }
                } 
            }
        }
    }

    /**
     * Puts all values in hash into FTL globals (#global).
     * <p>
     * @see copyMapToSimple
     */
    public static void globalsPutAll(TemplateHashModel hashModel, Set<String> inExKeys, Boolean include, Boolean onlyDirectives, Environment env) throws TemplateModelException {
        varsPutAll(hashModel, inExKeys, include, onlyDirectives, new GlobalFtlVarHandler(env), env);
    }

    /**
     * Puts all values in hash into FTL current namespace vars (#assign).
     * <p>
     * @see copyMapToSimple
     */
    public static void varsPutAll(TemplateHashModel hashModel, Set<String> inExKeys, Boolean include, Boolean onlyDirectives, Environment env) throws TemplateModelException {
        varsPutAll(hashModel, inExKeys, include, onlyDirectives, new CurrentFtlVarHandler(env), env);
    }

    /**
     * Puts all values in hash into FTL locals (#local).
     * <p>
     * @see copyMapToSimple
     */
    public static void localsPutAll(TemplateHashModel hashModel, Set<String> inExKeys, Boolean include, Boolean onlyDirectives, Environment env) throws TemplateModelException {
        varsPutAll(hashModel, inExKeys, include, onlyDirectives, new LocalFtlVarHandler(env), env);
    }
    
}
