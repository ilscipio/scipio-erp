package org.ofbiz.widget.renderer;

import java.util.Locale;
import java.util.Map;

import org.apache.catalina.servlet4preview.http.HttpServletRequest;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.renderer.RenderOptions;

/**
 * SCIPIO: An optional, dedicated class for render options stored in the
 * context and request.
 * <p>
 * Designed to avoid repeated lookups all the way through the MapStack
 * especially for frequent operations like logging statements.
 * <p>
 * NOT thread-safe.
 * <p>
 * WARN: It is the renderer population/setup code's job to ensure the request and context
 * instances are one and the same.
 * <p>
 * Added 2018-09-06.
 */
@SuppressWarnings("serial")
public class WidgetRenderOptions extends RenderOptions {

    protected Boolean warnMissingRenderer;
    // NOTE: warnMissingScreenRenderer not needed because it's always required otherwise errors
    protected Boolean warnMissingFormRenderer;
    protected Boolean warnMissingMenuRenderer;
    protected Boolean warnMissingTreeRenderer;

    /**
     * Default constructor.
     */
    protected WidgetRenderOptions() {
    }

    /**
     * Copy constructor.
     */
    protected WidgetRenderOptions(WidgetRenderOptions other) {
        this.warnMissingRenderer = other.warnMissingRenderer;
        this.warnMissingFormRenderer = other.warnMissingFormRenderer;
        this.warnMissingMenuRenderer = other.warnMissingMenuRenderer;
        this.warnMissingTreeRenderer = other.warnMissingTreeRenderer;
    }

    public static WidgetRenderOptions create(Delegator delegator, LocalDispatcher dispatcher, Locale locale) {
        return new WidgetRenderOptions();
    }

    public static WidgetRenderOptions create(DispatchContext dctx, Locale locale) {
        return new WidgetRenderOptions();
    }

    public static WidgetRenderOptions create(HttpServletRequest request) {
        return new WidgetRenderOptions();
    }

    /**
     * Creates a new instance using system defaults.
     * WARN: prefer the overloads with delegator and request.
     */
    public static WidgetRenderOptions create() {
        return new WidgetRenderOptions();
    }

    /**
     * Returns a read-only instance of the default system RenderOptions.
     */
    public static WidgetRenderOptions getDefaults() {
        return ReadOnlyRenderOptions.defaultOptions;
    }

    public WidgetRenderOptions copy() {
        return new WidgetRenderOptions(this);
    }

    public WidgetRenderOptions getReadOnly() {
        return new ReadOnlyRenderOptions(this);
    }

    // TODO?: fromRequest/fromContext (fromRequestOrNew/fromContextOrNew) that creates new if not present...
    // for now this is too complicated due to different request/context/globalContext/MapStack setups

    /**
     * Returns from context only if it was set by the renderer setup/populate.
     */
    public static WidgetRenderOptions fromContextIfPresent(Map<String, Object> context) {
        return (WidgetRenderOptions) context.get(RenderOptions.FIELD_NAME);
    }

    /**
     * Returns from request only if it was set by the renderer setup/populate.
     */
    public static WidgetRenderOptions fromRequestIfPresent(HttpServletRequest request) {
        return (WidgetRenderOptions) request.getAttribute(RenderOptions.FIELD_NAME);
    }

    /**
     * Returns from context OR read-only defaults.
     * ONLY use this if you do not need to modify RenderOptions (and system
     * defaults are acceptable).
     */
    public static WidgetRenderOptions fromContextOrDefaultsReadOnly(Map<String, Object> context) {
        WidgetRenderOptions renderOptions = (WidgetRenderOptions) context.get(RenderOptions.FIELD_NAME);
        return (renderOptions != null) ? renderOptions : getDefaults();
    }

    /**
     * Returns from request OR read-only defaults.
     * ONLY use this if you do not need to modify RenderOptions (and system
     * defaults are acceptable).
     */
    public static WidgetRenderOptions fromRequestOrDefaultsReadOnly(HttpServletRequest request) {
        WidgetRenderOptions renderOptions = (WidgetRenderOptions) request.getAttribute(RenderOptions.FIELD_NAME);
        return (renderOptions != null) ? renderOptions : getDefaults();
    }

    public static void setInContext(Map<String, Object> context, WidgetRenderOptions options) {
        context.put(RenderOptions.FIELD_NAME, options);
    }

    public static void setInRequest(HttpServletRequest request, WidgetRenderOptions options) {
        request.setAttribute(RenderOptions.FIELD_NAME, options);
    }

    public Boolean getWarnMissingRenderer() {
        return warnMissingRenderer;
    }

    public boolean isWarnMissingRendererEffective() {
        return Boolean.TRUE.equals(warnMissingRenderer);
    }

    public WidgetRenderOptions setWarnMissingRenderer(Boolean warnMissingRenderer) {
        this.warnMissingRenderer = warnMissingRenderer;
        return this;
    }

    public Boolean getWarnMissingFormRenderer() {
        return warnMissingFormRenderer;
    }

    public boolean isWarnMissingFormRendererEffective() {
        return (warnMissingFormRenderer != null) ? warnMissingFormRenderer : isWarnMissingRendererEffective();
    }

    public WidgetRenderOptions setWarnMissingFormRenderer(Boolean warnMissingFormRenderer) {
        this.warnMissingFormRenderer = warnMissingFormRenderer;
        return this;
    }

    public Boolean getWarnMissingMenuRenderer() {
        return warnMissingMenuRenderer;
    }

    public boolean isWarnMissingMenuRendererEffective() {
        return (warnMissingMenuRenderer != null) ? warnMissingMenuRenderer : isWarnMissingRendererEffective();
    }

    public WidgetRenderOptions setWarnMissingMenuRenderer(Boolean warnMissingMenuRenderer) {
        this.warnMissingMenuRenderer = warnMissingMenuRenderer;
        return this;
    }

    public Boolean getWarnMissingTreeRenderer() {
        return warnMissingTreeRenderer;
    }

    public boolean isWarnMissingTreeRendererEffective() {
        return (warnMissingTreeRenderer != null) ? warnMissingTreeRenderer : isWarnMissingRendererEffective();
    }

    public WidgetRenderOptions setWarnMissingTreeRenderer(Boolean warnMissingTreeRenderer) {
        this.warnMissingTreeRenderer = warnMissingTreeRenderer;
        return this;
    }

    protected static class ReadOnlyRenderOptions extends WidgetRenderOptions {

        private static final WidgetRenderOptions defaultOptions = new ReadOnlyRenderOptions();

        protected ReadOnlyRenderOptions() {
        }

        protected ReadOnlyRenderOptions(WidgetRenderOptions other) {
            super(other);
        }

        @Override
        public WidgetRenderOptions setWarnMissingRenderer(Boolean warnMissingRenderer) {
            throw new UnsupportedOperationException("Cannot modify read-only default RenderOptions");
        }

        @Override
        public WidgetRenderOptions setWarnMissingFormRenderer(Boolean warnMissingFormRenderer) {
            throw new UnsupportedOperationException("Cannot modify read-only default RenderOptions");
        }

        @Override
        public WidgetRenderOptions setWarnMissingMenuRenderer(Boolean warnMissingMenuRenderer) {
            throw new UnsupportedOperationException("Cannot modify read-only default RenderOptions");
        }

        @Override
        public WidgetRenderOptions setWarnMissingTreeRenderer(Boolean warnMissingTreeRenderer) {
            throw new UnsupportedOperationException("Cannot modify read-only default RenderOptions");
        }
    }
}
