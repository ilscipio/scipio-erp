package org.ofbiz.widget.model;

import org.ofbiz.widget.renderer.ScreenStringRenderer;

import java.util.Map;

/**
 * SCIPIO: May be thrown from {@link ModelScreenWidget#renderWidgetString(Appendable, Map, ScreenStringRenderer)} and other widget render
 * code in order to wrap existing exceptions.
 * <em>NOTE:</em> WidgetRenderException extends RuntimeException - contrary to ScreenRenderException.
 * Added 2019-05-21.
 */
public class WidgetRenderException extends RuntimeException {
    private final ModelWidget widget; // TODO: REVIEW: may wish to save the WidgetMetaInfo here instead...

    public WidgetRenderException() {
        this.widget = null;
    }

    public WidgetRenderException(String message) {
        super(message);
        this.widget = null;
    }

    public WidgetRenderException(String message, Throwable cause) {
        super(message, cause);
        this.widget = null;
    }

    public WidgetRenderException(Throwable cause) {
        super(cause);
        this.widget = null;
    }

    public WidgetRenderException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
        this.widget = null;
    }

    public WidgetRenderException(ModelWidget widget) {
        this.widget = widget;
    }

    public WidgetRenderException(String message, ModelWidget widget) {
        super(message);
        this.widget = widget;
    }

    public WidgetRenderException(String message, Throwable cause, ModelWidget widget) {
        super(message, cause);
        this.widget = widget;
    }

    public WidgetRenderException(Throwable cause, ModelWidget widget) {
        super(cause);
        this.widget = widget;
    }

    public WidgetRenderException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, ModelWidget widget) {
        super(message, cause, enableSuppression, writableStackTrace);
        this.widget = widget;
    }

    // DEV NOTE: the context argument is for internal use and information only; do not save as instance member at current time (2019-05-21)
    public WidgetRenderException(ModelWidget widget, Map<String, ?> context) {
        this.widget = widget;
    }

    public WidgetRenderException(String message, ModelWidget widget, Map<String, ?> context) {
        super(message);
        this.widget = widget;
    }

    public WidgetRenderException(String message, Throwable cause, ModelWidget widget, Map<String, ?> context) {
        super(message, cause);
        this.widget = widget;
    }

    public WidgetRenderException(Throwable cause, ModelWidget widget, Map<String, ?> context) {
        super(cause);
        this.widget = widget;
    }

    public WidgetRenderException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace, ModelWidget widget, Map<String, ?> context) {
        super(message, cause, enableSuppression, writableStackTrace);
        this.widget = widget;
    }

    /**
     * Returns the widget in which the exception occurred, or null if not available or not applicable.
     * NOTE: this is best-effort and may be null even in places where you would expect there to be a widget instance;
     * you can get widget information in a better way by getter {@link ModelWidget.WidgetMetaInfo} through {@link #getWidgetInfo()}.
     */
    public ModelWidget getWidget() {
        return widget;
    }

    public static ModelWidget getWidget(Throwable t) {
        return (t instanceof WidgetRenderException) ? ((WidgetRenderException) t).getWidget() : null;
    }

    /**
     * Returns the widget meta info in which the exception occurred, or WidgetMetaInfo.UNDEFINED if not available or not applicable.
     * NOTE: this is best-effort and may be UNDEFINED even in places where you would expect there to be a widget instance.
     */
    public ModelWidget.WidgetMetaInfo getWidgetInfo() {
        return (getWidget() != null) ? getWidget().getMetaInfo() : ModelWidget.WidgetMetaInfo.UNDEFINED;
    }

    public String getMessage() {
        return (getWidget() != null) ? super.getMessage()  + " " + getWidget().getMetaInfo() : super.getMessage();
    }

    /**
     * Returns the message, without any additional widget info.
     */
    protected String getRawMessage() {
        return super.getMessage();
    }
}
