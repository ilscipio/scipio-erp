package org.ofbiz.webapp.renderer;

import java.util.Map;

import javax.servlet.ServletRequest;

/**
 * SCIPIO: Provides information about the current render.
 * <p>
 * This interface is essential in order to make information
 * available to webapp component from widget package that we can't reference.
 * <p>
 * 2018-08-16: Currently this info is provided by the 
 * {@link org.ofbiz.widget.renderer.ScreenRenderer} class, because
 * it's the only thing available everywhere. But this is
 * Subject to change in the future! The implementations
 * of the fromRequest and fromContext methods below may change.
 */
public interface RenderInfo {

    /**
     * Returns the renderer name; usually "html", "xsl-fo",
     * "xml", "csv", etc. (as used on right side of xxxx.name= in widget.properties).
     */
    public String getRendererName();
    
    @SuppressWarnings("unchecked")
    public static <T extends RenderInfo> T fromRequest(ServletRequest request) {
        return (T) request.getAttribute("screens");
    }
    
    @SuppressWarnings("unchecked")
    public static <T extends RenderInfo> T fromContext(Map<String, Object> context, RenderEnvType renderEnvType) {
        return (T) context.get("screens");
    }
    
    @SuppressWarnings("unchecked")
    public static <T extends RenderInfo> T fromContext(Map<String, Object> context) {
        return (T) context.get("screens");
    }
}
