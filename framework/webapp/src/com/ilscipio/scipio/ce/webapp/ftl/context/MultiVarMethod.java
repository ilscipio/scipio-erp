package com.ilscipio.scipio.ce.webapp.ftl.context;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.template.FreeMarkerWorker;

import com.ilscipio.scipio.ce.webapp.ftl.lang.LangFtlUtil;

import freemarker.core.Environment;
import freemarker.template.SimpleHash;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateBooleanModel;
import freemarker.template.TemplateCollectionModel;
import freemarker.template.TemplateHashModelEx;
import freemarker.template.TemplateMethodModelEx;
import freemarker.template.TemplateModel;
import freemarker.template.TemplateModelException;
import freemarker.template.TemplateSequenceModel;

/**
 * <code>#extractVars</code>, <code>#setVars</code>, <code>#clearVars</code> function implementations.
 */
public abstract class MultiVarMethod implements TemplateMethodModelEx {

    public MultiVarMethod() {
    }

    @SuppressWarnings({ "rawtypes", "unchecked" })
    @Override
    public Object exec(List args) throws TemplateModelException {
        return execTyped(args);
    }

    protected abstract Object execTyped(List<TemplateModel> args) throws TemplateModelException;

    public static class ExtractVarsMethod extends MultiVarMethod {
        @Override
        protected Object execTyped(List<TemplateModel> args) throws TemplateModelException {
            TemplateHashModelEx varListsModel = (TemplateHashModelEx) args.get(0);
            TemplateBooleanModel saveNullsModel = TemplateBooleanModel.TRUE;
            if (args.size() >= 2) {
                saveNullsModel = (TemplateBooleanModel) args.get(1);
            }
            CommonVarMaps<Collection<String>> varLists = CommonVarMaps.getRawSequences(varListsModel);
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            CommonVarMaps<Map<String, Object>> res = extractVars(varLists, saveNullsModel.getAsBoolean(), env);
            return res.toMapModel(env);
        }
        
        public static CommonVarMaps<Map<String, Object>> extractVars(CommonVarMaps<Collection<String>> keys, boolean saveNulls, Environment env) throws TemplateModelException {
            CommonVarMaps<Map<String, Object>> res = new CommonVarMaps<>();
            
            if (keys.ctxVars != null && !keys.ctxVars.isEmpty()) {
                res.ctxVars = new HashMap<>();
                Map<String, Object> context = ContextFtlUtil.getContext(env);
                for(String key : keys.ctxVars) {
                    Object value = context.get(key);
                    if (value != null || saveNulls) {
                        res.ctxVars.put(key, value);
                    }
                }
            }
            
            if (keys.globalCtxVars != null && !keys.globalCtxVars.isEmpty()) {
                res.globalCtxVars = new HashMap<>();
                Map<String, Object> globalContext = ContextFtlUtil.getGlobalContext(env);
                for(String key : keys.globalCtxVars) {
                    Object value = globalContext.get(key);
                    if (value != null || saveNulls) {
                        res.globalCtxVars.put(key, value);
                    }        
                }
            }
            
            if (keys.reqAttribs != null && !keys.reqAttribs.isEmpty()) {
                res.reqAttribs = new HashMap<>();
                HttpServletRequest request = ContextFtlUtil.getRequest(env);
                for(String key : keys.reqAttribs) {
                    Object value = request.getAttribute(key);
                    if (value != null || saveNulls) {
                        res.reqAttribs.put(key, value);
                    } 
                }
            }
            return res;
        }
    }

    public static class SetVarsMethod extends MultiVarMethod {
        @Override
        protected Object execTyped(List<TemplateModel> args) throws TemplateModelException {
            TemplateHashModelEx varMapsModel = (TemplateHashModelEx) args.get(0);
            CommonVarMaps<Map<String, Object>> varMaps = CommonVarMaps.getRawMaps(varMapsModel);
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            setVars(varMaps, env);
            return new SimpleScalar("");
        }
        
        public static void setVars(CommonVarMaps<Map<String, Object>> maps, Environment env) throws TemplateModelException {
            setVars(maps, CommonVarMaps.getAllFalse(), env);
        }
        
        public static void setVars(CommonVarMaps<Map<String, Object>> maps, CommonVarMaps<Boolean> skipSet, Environment env) throws TemplateModelException {
            if (maps.ctxVars != null && !maps.ctxVars.isEmpty() && !skipSet.ctxVars) {
                Map<String, Object> context = ContextFtlUtil.getContext(env);
                context.putAll(maps.ctxVars);
            }
            
            if (maps.globalCtxVars != null && !maps.globalCtxVars.isEmpty() && !skipSet.globalCtxVars) {
                Map<String, Object> globalContext = ContextFtlUtil.getGlobalContext(env);
                globalContext.putAll(maps.globalCtxVars);
            }
            
            if (maps.reqAttribs != null && !maps.reqAttribs.isEmpty() && !skipSet.reqAttribs) {
                HttpServletRequest request = ContextFtlUtil.getRequest(env);
                for(Map.Entry<String, Object> entry : maps.reqAttribs.entrySet()) {
                    request.setAttribute(entry.getKey(), entry.getValue());
                }
            }
        }
    }
    
    public static class ClearVarsMethod extends MultiVarMethod {
        @Override
        protected Object execTyped(List<TemplateModel> args) throws TemplateModelException {
            TemplateHashModelEx varListsModel = (TemplateHashModelEx) args.get(0);
            CommonVarMaps<Collection<String>> varLists = CommonVarMaps.getRawSequences(varListsModel);
            Environment env = FreeMarkerWorker.getCurrentEnvironment();
            clearVars(varLists, env);
            return new SimpleScalar("");
        }
        
        public static void clearVars(CommonVarMaps<Collection<String>> keys, Environment env) throws TemplateModelException {
            if (keys.ctxVars != null && !keys.ctxVars.isEmpty()) {
                Map<String, Object> context = ContextFtlUtil.getContext(env);
                for(String key : keys.ctxVars) {
                    context.remove(key);
                }
            }
            
            if (keys.globalCtxVars != null && !keys.globalCtxVars.isEmpty()) {
                Map<String, Object> globalContext = ContextFtlUtil.getGlobalContext(env);
                for(String key : keys.globalCtxVars) {
                    globalContext.remove(key) ;      
                }
            }
            
            if (keys.reqAttribs != null && !keys.reqAttribs.isEmpty()) {
                HttpServletRequest request = ContextFtlUtil.getRequest(env);
                for(String key : keys.reqAttribs) {
                    request.removeAttribute(key);
                }
            }
        }
    }
    
    public static class CommonVarMaps<T> {
        private static final CommonVarMaps<Boolean> allFalse = new CommonVarMaps<>(false, false, false);
        
        public T ctxVars;
        public T globalCtxVars;
        public T reqAttribs;
        
        public CommonVarMaps(T ctxVars, T globalCtxVars, T reqAttribs) {
            this.ctxVars = ctxVars;
            this.globalCtxVars = globalCtxVars;
            this.reqAttribs = reqAttribs;
        }

        public CommonVarMaps() {
        }
        
        public static CommonVarMaps<Boolean> getAllFalse() {
            return allFalse;
        }
        
        public static CommonVarMaps<Collection<String>> getRawSequences(TemplateHashModelEx varListsModel) throws TemplateModelException {
            return new CommonVarMaps<>(
                    getAsRawSequence(varListsModel.get("ctxVars")),
                    getAsRawSequence(varListsModel.get("globalCtxVars")),
                    getAsRawSequence(varListsModel.get("reqAttribs")));
        }
        
        public static CommonVarMaps<Collection<String>> getRawSequences(Map<String, TemplateModel> varLists) throws TemplateModelException {
            return new CommonVarMaps<>(
                    getAsRawSequence(varLists.get("ctxVars")),
                    getAsRawSequence(varLists.get("globalCtxVars")),
                    getAsRawSequence(varLists.get("reqAttribs")));
        }
        
        public static CommonVarMaps<Collection<String>> getRawSequences(CommonVarMaps<Map<String, Object>> varMaps) throws TemplateModelException {
            return new CommonVarMaps<>(varMaps.ctxVars != null ? varMaps.ctxVars.keySet() : Collections.<String>emptyList(),
                    varMaps.globalCtxVars != null ? varMaps.globalCtxVars.keySet() : Collections.<String>emptyList(),
                    varMaps.reqAttribs != null ? varMaps.reqAttribs.keySet() : Collections.<String>emptyList());
        }
        
        @SuppressWarnings("unchecked")
        public static CommonVarMaps<Map<String, Object>> getRawMaps(TemplateHashModelEx varMapsModel) throws TemplateModelException {
            return new CommonVarMaps<>(
                    (Map<String, Object>) LangFtlUtil.unwrap(varMapsModel.get("ctxVars")),
                    (Map<String, Object>) LangFtlUtil.unwrap(varMapsModel.get("globalCtxVars")),
                    (Map<String, Object>) LangFtlUtil.unwrap(varMapsModel.get("reqAttribs")));
        }
        
        @SuppressWarnings("unchecked")
        public static CommonVarMaps<Map<String, Object>> getRawMaps(Map<String, TemplateModel> varMaps) throws TemplateModelException {
            return new CommonVarMaps<>(
                    (Map<String, Object>) LangFtlUtil.unwrap(varMaps.get("ctxVars")),
                    (Map<String, Object>) LangFtlUtil.unwrap(varMaps.get("globalCtxVars")),
                    (Map<String, Object>) LangFtlUtil.unwrap(varMaps.get("reqAttribs")));
        }
        
        public Map<String, Object> toMap() {
            Map<String, Object> map = new HashMap<>();
            map.put("ctxVars", ctxVars);
            map.put("globalCtxVars", globalCtxVars);
            map.put("reqAttribs", reqAttribs);
            return map;
        }
        
        public TemplateHashModelEx toMapModel(Environment env) {
            SimpleHash map = new SimpleHash(env.getObjectWrapper());
            map.put("ctxVars", ctxVars);
            map.put("globalCtxVars", globalCtxVars);
            map.put("reqAttribs", reqAttribs);
            return map;
        }
    }
    
    @SuppressWarnings("unchecked")
    static Collection<String> getAsRawSequence(TemplateModel elems) throws TemplateModelException {
        if (elems == null || elems instanceof TemplateBooleanModel) {
            return Collections.emptyList();
        } else if (elems instanceof TemplateCollectionModel || elems instanceof TemplateSequenceModel) {
            return (Collection<String>) LangFtlUtil.unwrapAlways(elems);
        } else if (elems instanceof TemplateHashModelEx) {
            return (Collection<String>) LangFtlUtil.getMapKeys(elems);
        } else {
            throw new TemplateModelException("invalid parameter type, can't interpret as sequence: " + elems.getClass());
        }
    }
    
}
