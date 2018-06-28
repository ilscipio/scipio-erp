package com.ilscipio.scipio.ce.webapp.ftl.lang;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import freemarker.ext.beans.BeanModel;
import freemarker.ext.util.WrapperTemplateModel;
import freemarker.template.SimpleScalar;
import freemarker.template.TemplateHashModel;
import freemarker.template.TemplateModel;

/**
 * SCIPIO: Type descriptions for common FTL object types as seen from templating code.
 * <p>
 * Needed because the standard FTL type names don't cover the special cases caused by
 * bean wrapping of the ofbiz screen context vars. Essentially Ofbiz-specific.
 * <p>
 * <em>NOTE</em>: this is mainly needed for maps. Sequences don't suffer from the
 * same type confusion or they can be examined using the expected type checks (for the most part).
 * <p>
 * TODO: REVIEW: 2017: these checks must be reviewed for non BeanModel wrappers...
 */
public enum OfbizFtlObjectType {
    
    /**
     * Anything meant to be a string WITHOUT being a more complex type.
     */
    STRING("string") {
        @Override
        public boolean isObjectType(TemplateModel object) {
            if (object instanceof SimpleScalar) {
                return true;
            }
            else if (object instanceof WrapperTemplateModel) {
                Object wrappedObject = ((WrapperTemplateModel) object).getWrappedObject();
                if (wrappedObject instanceof CharSequence) {
                    return true;
                }
            }
            return false;
        }
    },
    
    /**
     * Simple hash, or context map that exposes methods as keys (BeanModel with underlying Map)
     * (simplemap or complexmap).
     */
    MAP("map") {
        @Override
        public boolean isObjectType(TemplateModel object) {
            if (object instanceof TemplateHashModel) {
                if (object instanceof WrapperTemplateModel) {
                    Object wrappedObject = ((WrapperTemplateModel) object).getWrappedObject();
                    if (wrappedObject instanceof Map) { // in ofbiz context, this could be string or other
                        return true;
                    }
                }
                else {
                    // doesn't wrap anything, so probably a simple hash model of some sort
                    return true;
                }
            }
            return false;
        }
    },
    
    /**
     * Simple hash only (?keys to get elems).
     */
    SIMPLEMAP("simplemap") {
        @Override
        public boolean isObjectType(TemplateModel object) {
            if (object instanceof TemplateHashModel && !(object instanceof BeanModel)) {
                // this is an approximation, works for now for ofbiz, but could break on some types (FIXME?)
                return true;
            }
            return false;
        }
    },
    
    /**
     * Context map that exposes methods as keys (BeanModel) only (.keySet() to get elems).
     */
    COMPLEXMAP("complexmap") {
        @Override
        public boolean isObjectType(TemplateModel object) {
            if (object instanceof TemplateHashModel && object instanceof BeanModel) {
                Object wrappedObject = ((WrapperTemplateModel) object).getWrappedObject();
                if (wrappedObject instanceof Map) { // in ofbiz context, this could be string or other
                    return true;
                }
            }
            return false;
        }
    },
    
    /**
     * Collection wrapped in a bean model.
     */
    COMPLEXCOLLECTION("complexcollection") {
        @Override
        public boolean isObjectType(TemplateModel object) {
            if (object instanceof BeanModel) {
                Object wrappedObject = ((WrapperTemplateModel) object).getWrappedObject();
                if (wrappedObject instanceof Collection) { // in ofbiz context, this could be string or other
                    return true;
                }
            }
            return false;
        }
    },
    
    /**
     * Anything wrapped in a bean model.
     */
    COMPLEXOBJECT("complexobject") {
        @Override
        public boolean isObjectType(TemplateModel object) {
            if (object instanceof BeanModel) {
                return true;
            }
            return false;
        }
    };
    
    private static final Map<String, OfbizFtlObjectType> ftlNameHash;
    
    static {
        Map<String, OfbizFtlObjectType> map = new HashMap<>();
        for (OfbizFtlObjectType type : OfbizFtlObjectType.values()) {
            map.put(type.getFtlName(), type);
        }
        ftlNameHash = map;
    }
    
    private final String fltName;

    private OfbizFtlObjectType(String fltName) {
        this.fltName = fltName;
    }
    
    public String getFtlName() {
        return fltName;
    }
    
    public static OfbizFtlObjectType fromFtlNameSafe(String ftlName) {
        return ftlNameHash.get(ftlName);
    }
    
    /**
     * Checks if the given model matches this logical FTL object type.
     */
    public abstract boolean isObjectType(TemplateModel object);
    
    /**
     * Checks if the given model matches the logical FTL object type.
     */
    public static boolean isObjectType(OfbizFtlObjectType ftlType, TemplateModel object) {
        if (ftlType != null) {
            return ftlType.isObjectType(object);
        }
        return false;
    }

    public static boolean isObjectTypeSafe(String ftlTypeName, TemplateModel object) {
        return isObjectType(OfbizFtlObjectType.fromFtlNameSafe(ftlTypeName), object);
    }

}