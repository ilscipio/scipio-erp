package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.HashMap;
import java.util.Map;

import freemarker.core.Environment;
import freemarker.template.ObjectWrapper;
import freemarker.template.TemplateModelException;

public class WrappingOptions {

    protected final String targetWrapperName;
    protected final ObjectWrapper targetWrapper;
    protected final RewrapMode rewrapMode;
    
    protected final ObjectWrapper curObjectWrapper;

    public WrappingOptions(String targetWrapperName, ObjectWrapper targetWrapper, RewrapMode rewrapMode, ObjectWrapper curObjectWrapper) {
        super();
        this.targetWrapperName = targetWrapperName;
        this.targetWrapper = targetWrapper;
        this.rewrapMode = rewrapMode;
        this.curObjectWrapper = curObjectWrapper;
    }
    
    public static WrappingOptions makeOptions(String wrapper, String mode, Environment env) throws TemplateModelException {
        ObjectWrapper targetWrapper = ObjectWrapperUtil.getObjectWrapperByName(wrapper, env);
        if (targetWrapper == null) {
            throw new TemplateModelException("Unrecognized wrapper name: " + wrapper);
        }
        if (wrapper == null) {
            wrapper = "";
        }
        
        RewrapMode rewrapMode = RewrapMode.ALWAYS_DEEP;
        if (mode != null && !mode.isEmpty()) {
            rewrapMode = RewrapMode.fromString(mode);
            if (rewrapMode == null) {
                throw new TemplateModelException("Unrecognized rewrapping mode: " + mode);
            }
        }
        
        return new WrappingOptions(wrapper, ObjectWrapperUtil.getObjectWrapperByName(wrapper, env), 
                rewrapMode, env.getObjectWrapper());
    }

    public String getTargetWrapperName() {
        return targetWrapperName;
    }

    public ObjectWrapper getTargetWrapper() {
        return targetWrapper;
    }

    public RewrapMode getRewrapMode() {
        return rewrapMode;
    }

    public ObjectWrapper getCurObjectWrapper() {
        return curObjectWrapper;
    }

    /**
     * Rewrap mode for rewrapObject macro (third parameter).
     */
    public enum RewrapMode {
        
        // TODO: all the commented ones
        //ALWAYS("always", true, false, false),
        ALWAYS_DEEP("always-deep", true, true, false);
        
        //NEEDED("needed", false, false, false),
        //NEEDED_DEEP("needed-deep", false, true, false),
        
        //NEEDED_FAST("needed", false, false, true),
        //NEEDED_DEEP_FAST("needed-deep", false, true, true);
      
        private static final Map<String, RewrapMode> strToModeMap;
        static {
            Map<String, RewrapMode> modeMap = new HashMap<String, RewrapMode>();
            for(RewrapMode mode : RewrapMode.values()) {
                modeMap.put(mode.strVal, mode);
            }
            strToModeMap = modeMap;
        }

        final String strVal;
        final boolean always; // where to always force rewrap (true), or try to avoid (false)
        final boolean deep;
        final boolean fast;
        
        private RewrapMode(String strVal, boolean always, boolean deep, boolean fast) {
            this.strVal = strVal;
            this.always = always;
            this.deep = deep;
            this.fast = fast;
        }

        public static RewrapMode fromString(String str) {
            return strToModeMap.get(str);
        }

        public String getStrVal() {
            return strVal;
        }

        public boolean isAlways() {
            return always;
        }

        public boolean isDeep() {
            return deep;
        }
        
        public boolean isFast() {
            return fast;
        }
    }
}
