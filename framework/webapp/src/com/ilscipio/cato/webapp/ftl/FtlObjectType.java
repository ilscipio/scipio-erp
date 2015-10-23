package com.ilscipio.cato.webapp.ftl;

import java.util.Map;

import javolution.util.FastMap;

public enum FtlObjectType {
    STRING("string"), // anything meant to be a string without being a more complex type
    MAP("map"), // simple hash or context map that exposes methods as keys (BeanModel)
    SIMPLEMAP("simplemap"), // simple FTL hash only
    COMPLEXMAP("complexmap"); // context map that exposes methods as keys (BeanModel) only
    
    private static final Map<String, FtlObjectType> ftlNameHash;
    
    static {
        ftlNameHash = FastMap.newInstance();
        for (FtlObjectType type : FtlObjectType.values()) {
            ftlNameHash.put(type.getFtlName(), type);
        }
    }
    
    private final String fltName;

    private FtlObjectType(String fltName) {
        this.fltName = fltName;
    }
    
    public String getFtlName() {
        return fltName;
    }
    
    public static FtlObjectType fromFtlNameSafe(String ftlName) {
        return ftlNameHash.get(ftlName);
    }
}