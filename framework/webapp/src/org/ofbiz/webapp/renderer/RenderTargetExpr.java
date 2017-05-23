package org.ofbiz.webapp.renderer;

import java.io.Serializable;
import java.util.Map;

/**
 * SCIPIO: compiled targeted rendering expression base interface.
 * DEV NOTE: require this because we don't have access to widget package from here.
 */
public interface RenderTargetExpr extends RenderTargetExprBase {

    public interface MultiRenderTargetExpr<T extends RenderTargetExpr> extends Map<String, T>, RenderTargetExprBase {
        
    }
    
    public interface RenderTargetState extends Serializable {
        RenderTargetExprBase getExpr();
    }
}
