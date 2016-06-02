package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.HashMap;
import java.util.Map;

public enum RewrapMode {
    
    // Allows listing exactly all the modes supported
    SIMPLE("simple", true, false, false, false, false),
    SIMPLE_RAW_DEEP("simple-raw-deep", true, true, true, false, false),
    
    SIMPLE_FORCE("simple-force", true, false, false, true, false),
    SIMPLE_RAW_DEEP_FORCE("simple-raw-deep-force", true, true, true, true, false),
    
    SIMPLE_COPY("simple-copy", true, false, false, false, true),
    SIMPLE_RAW_DEEP_COPY("simple-raw-deep-copy", true, true, true, false, true),
    
    SIMPLE_FORCE_COPY("simple-force-copy", true, false, false, true, true),
    SIMPLE_RAW_DEEP_FORCE_COPY("simple-raw-deep-force-copy", true, true, true, true, true);
    
    private static final Map<String, RewrapMode> strToModeMap;
    static {
        Map<String, RewrapMode> modeMap = new HashMap<String, RewrapMode>();
        for(RewrapMode mode : RewrapMode.values()) {
            modeMap.put(mode.strVal, mode);
        }
        strToModeMap = modeMap;
    }

    final String strVal;
    final boolean simple;
    final boolean raw;
    final boolean deep;
    final boolean force;
    final boolean copy;
    
    private RewrapMode(String strVal, boolean simple, boolean raw, boolean deep, boolean force, boolean copy) {
        this.strVal = strVal;
        this.simple = simple;
        this.raw = raw;
        this.deep = deep;
        this.force = force;
        this.copy = copy;
    }

    public static RewrapMode fromString(String str) {
        return strToModeMap.get(str);
    }

    public String getStrVal() {
        return strVal;
    }

    public boolean isSimple() {
        return simple;
    }

    public boolean isRaw() {
        return raw;
    }

    public boolean isDeep() {
        return deep;
    }

    public boolean isForce() {
        return force;
    }
    
    public boolean isCopy() {
        return copy;
    }
}