package org.ofbiz.widget.renderer;

import java.util.Map;

/**
 * RenderContextWorker (SCIPIO).
 */
public class RenderContextWorker {

    public static String getRenderPlatformType(Map<String, ?> context) {
        ScreenRenderer screens = (ScreenRenderer) context.get("screens");
        if (screens != null) {
            return screens.getScreenStringRenderer().getRendererName();
        }
        return "default";
    }

    public static String getRenderContextType(Map<String, ?> context) {
        return (context.get("request") != null) ? "web" : (context.get("baseUrl") != null ? "email" : "general");
    }
}
