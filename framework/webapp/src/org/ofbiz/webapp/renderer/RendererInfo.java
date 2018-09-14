package org.ofbiz.webapp.renderer;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

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
public interface RendererInfo {

    /**
     * Returns the renderer name; usually "html", "xsl-fo",
     * "xml", "csv", etc. (as used on right side of xxxx.name= in widget.properties).
     */
    String getRendererName();

    @SuppressWarnings("unchecked")
    public static <T extends RendererInfo> T fromRequest(HttpServletRequest request) {
        return (T) request.getAttribute("screens"); // SUBJECT TO CHANGE
    }

    @SuppressWarnings("unchecked")
    public static <T extends RendererInfo> T fromContext(Map<String, Object> context, RenderEnvType renderEnvType) {
        return (T) context.get("screens"); // SUBJECT TO CHANGE
    }

    public static <T extends RendererInfo> T fromContext(Map<String, Object> context) {
        return fromContext(context, null);
    }

    public static <T extends RendererInfo> T fromRequestOrContext(HttpServletRequest request, Map<String, Object> context) {
        return (request != null) ? fromRequest(request) : (context != null) ? fromContext(context) : null;
    }
}
