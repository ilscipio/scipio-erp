package org.ofbiz.webapp.renderer;

/**
 * SCIPIO
 */
public enum RenderEnvType {
    WEBAPP,
    /**
     * NOTE: at current time, email counts toward isStatic;
     * however in future other solutions are possible.
     */
    EMAIL,
    STATIC;
    
    public boolean isWebapp() {
        return this == WEBAPP;
    }
    
    public boolean isStatic() {
        return this == EMAIL || this == STATIC;
    }
}
