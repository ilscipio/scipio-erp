package org.ofbiz.widget.renderer;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.webapp.renderer.RenderEnvType;

import java.util.Map;

/**
 * RenderEnvWorker (SCIPIO).
 */
public class RenderEnvWorker {

    public static String getRenderContextType(Map<String, ?> context) {
        return RenderEnvType.fromContext(UtilGenerics.cast(context)).getApiName();
    }

    public static String getRenderPlatformType(Map<String, ?> context) {
        ScreenRenderer screens = (ScreenRenderer) context.get("screens");
        if (screens != null) {
            return screens.getScreenStringRenderer().getRendererName();
        }
        return "default";
    }
}
