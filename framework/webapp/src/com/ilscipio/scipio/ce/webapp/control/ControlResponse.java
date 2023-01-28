package com.ilscipio.scipio.ce.webapp.control;

import java.io.Serializable;
import java.util.Map;
import java.util.Set;

import org.ofbiz.webapp.control.ConfigXMLReader.RequestResponse;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestResponse.Type;
import org.ofbiz.webapp.control.RequestHandler;

/**
 * Defines responses supported by controllers and {@link RequestHandler} for controller event results, as a java
 * API wrapping the controller response model.
 *
 * <p>Traditionally, events only returned response names, but these ControlResponse objects may now be returned
 * instead of response names for arbitrary and complex flow control and to help support annotations.</p>
 *
 * <p>NOTE: These responses are only valid to be returned from {@link com.ilscipio.scipio.ce.webapp.control.Request}
 * events and controller <code>request-map</code> events, and not preprocessor/postprocessor or other types of events.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support and versatile events.</p>
 */
public abstract class ControlResponse implements Serializable {

    protected transient RequestResponse requestResponse;

    protected String responseName;
    protected boolean error;
    protected Integer statusCode;
    protected Boolean saveLastView;
    protected Boolean saveCurrentView;
    protected Boolean saveHomeView;

    protected Map<String, String> redirectParameterMap;
    protected Map<String, String> redirectParameterValueMap;
    protected Set<String> excludeParameterSet;
    protected String includeMode;
    protected Boolean allowViewSave;
    protected String saveRequest;
    protected Set<String> includeRequestAttributes;
    protected Set<String> excludeRequestAttributes;
    protected String connectionState;
    protected Boolean allowCacheRedirect;

    protected ControlResponse() {
    }

    protected ControlResponse(ControlResponse other) {
        this.responseName = other.responseName;
        this.error = other.error;
        this.statusCode = other.statusCode;
        this.saveLastView = other.saveLastView;
        this.saveCurrentView = other.saveCurrentView;
        this.saveHomeView = other.saveHomeView;
        this.redirectParameterMap = other.redirectParameterMap;
        this.redirectParameterValueMap = other.redirectParameterValueMap;
        this.excludeParameterSet = other.excludeParameterSet;
        this.includeMode = other.includeMode;
        this.allowViewSave = other.allowViewSave;
        this.saveRequest = other.saveRequest;
        this.includeRequestAttributes = other.includeRequestAttributes;
        this.excludeRequestAttributes = other.excludeRequestAttributes;
        this.connectionState = other.connectionState;
        this.allowCacheRedirect = other.allowCacheRedirect;
    }

    public abstract ControlResponse copy();

    /*
     * Factory methods for builder-like instances
     */

    /**
     * No-render response.
     */
    public static None none() {
        return new None();
    }

    /**
     * Specific view render.
     */
    public static View view(String viewName) {
        return new View(viewName);
    }

    /**
     * Last-view render, with specific viewName as default.
     */
    public static ViewLast viewLast(String viewName) {
        return new ViewLast(viewName);
    }

    /**
     * Last-view render, with specific viewName as default, with no automatically-retained parameters.
     */
    public static ViewLast viewLastNoParam(String viewName) {
        return new ViewLast(viewName).noParam(true);
    }

    /**
     * Home-view render, with specific viewName as default.
     */
    public static ViewHome viewHome(String viewName) {
        return new ViewHome(viewName);
    }

    /**
     * Specific controller request forward, with uri a controller request-map name.
     */
    public static Request request(String uri) {
        return new Request(uri);
    }

    /**
     * Controller request redirect, with uri a controller request-map name.
     */
    public static RequestRedirect requestRedirect(String uri) {
        return new RequestRedirect(uri);
    }

    /**
     * Controller request redirect, with uri a controller request-map name, without automatical request parameters.
     */
    public static RequestRedirect requestRedirectNoParam(String uri) {
        return new RequestRedirect(uri).noParam(true);
    }

    /**
     * Last controller request redirect (if implemented for webapp), with uri a controller request-map name as fallback.
     */
    public static RequestRedirectLast requestRedirectLast(String uri) {
        return new RequestRedirectLast(uri);
    }

    /**
     * Controller cross-webapp redirect, with uri a URI from server root (webapp mount-point plus path).
     */
    public static CrossRedirect crossRedirect(String uri) {
        return new CrossRedirect(uri);
    }

    /**
     * Any-url redirect.
     */
    public static UrlRedirect urlRedirect(String url) {
        return new UrlRedirect(url);
    }

    /*
     * Fields and builder methods
     */

    /**
     * Builds a {@link RequestResponse} from this instance for use in {@link RequestHandler}; client
     * code does not need to call as it is transiently cached on this instance.
     * <p>NOTE: This finalizes the instance and any further calls to setter will not modify this response.</p>
     */
    public RequestResponse asRequestResponse() {
        RequestResponse requestResponse = this.requestResponse;
        if (requestResponse == null) {
            requestResponse = makeRequestResponse();
            this.requestResponse = requestResponse;
        }
        return requestResponse;
    }

    public RequestResponse makeRequestResponse() {
        return new RequestResponse(this);
    }

    public abstract Type type();

    public String value() {
        return null;
    }

    public String responseName() {
        return responseName;
    }

    /**
     * Sets the response name; default "success", unless {@link #error(boolean)} is flagged in which case default "error".
     * <p>NOTE: Use {@link #error(boolean)} to mark error and if the responseName is not set, it will be automatically
     * set to "error".</p>
     */
    public ControlResponse responseName(String responseName) {
        this.responseName = responseName;
        return this;
    }

    public boolean error() {
        return error;
    }

    /**
     * Sets the error flag; unless overridden, this results in "error" response by name.
     */
    public ControlResponse error(boolean error) {
        this.error = error;
        return this;
    }

    public Boolean saveLastView() {
        return saveLastView;
    }

    /**
     * Saves the last (previous) request's view for future use, generally with the view-last type of response.
     */
    public ControlResponse saveLastView(Boolean saveLastView) {
        this.saveLastView = saveLastView;
        return this;
    }

    public Boolean saveCurrentView() {
        return saveCurrentView;
    }

    /**
     * Saves the current request's view for future use, generally with the view-last type of response.
     */
    public ControlResponse saveCurrentView(Boolean saveCurrentView) {
        this.saveCurrentView = saveCurrentView;
        return this;
    }

    public Boolean saveHomeView() {
        return saveHomeView;
    }

    /**
     * Saves the current request's view for future use, generally with the view-home type of response.
     */
    public ControlResponse saveHomeView(Boolean saveHomeView) {
        this.saveHomeView = saveHomeView;
        return this;
    }

    public Boolean allowViewSave() {
        return allowViewSave;
    }

    /**
     * If set to false (default true), explicitly prevents saving this view (to session), such that it will not be known to the
     * "view-last" type responses or any of the other "view-*" responses, and will not crush the previously saved view.
     * <p>If false, causes the other save-*-view attributes to be ignored.</p>
     * <p>NOTE: This can also be requested by passing "_ALLOW_VIEW_SAVE_=N" as request parameter.</p>
     */
    public ControlResponse allowViewSave(Boolean allowViewSave) {
        this.allowViewSave = allowViewSave;
        return this;
    }

    public RequestResponse requestResponse() {
        return requestResponse;
    }

    public Integer statusCode() {
        return statusCode;
    }

    public Boolean sveHomeView() {
        return saveHomeView;
    }

    public Map<String, String> redirectParameterMap() {
        return redirectParameterMap;
    }

    public Map<String, String> redirectParameterValueMap() {
        return redirectParameterValueMap;
    }

    public Set<String> excludeParameterSet() {
        return excludeParameterSet;
    }

    public String includeMode() {
        return includeMode;
    }

    public String saveRequest() {
        return saveRequest;
    }

    public Set<String> includeRequestAttributes() {
        return includeRequestAttributes;
    }

    public Set<String> excludeRequestAttributes() {
        return excludeRequestAttributes;
    }

    public String connectionState() {
        return connectionState;
    }

    public Boolean allowCacheRedirect() {
        return allowCacheRedirect;
    }

    /*
     * Response types
     */

    public static class None extends ControlResponse {
        public None() {
        }

        public None(None other) {
            super(other);
        }

        @Override
        public None copy() {
            return new None(this);
        }

        @Override
        public ControlResponse responseName(String responseName) {
            return super.responseName(responseName);
        }

        @Override
        public ControlResponse error(boolean error) {
            throw new UnsupportedOperationException();
        }

        @Override
        public ControlResponse saveLastView(Boolean saveLastView) {
            throw new UnsupportedOperationException();
        }

        @Override
        public ControlResponse saveCurrentView(Boolean saveCurrentView) {
            throw new UnsupportedOperationException();
        }

        @Override
        public ControlResponse saveHomeView(Boolean saveHomeView) {
            throw new UnsupportedOperationException();
        }

        @Override
        public ControlResponse allowViewSave(Boolean allowViewSave) {
            throw new UnsupportedOperationException();
        }

        @Override
        public Type type() {
            return Type.NONE;
        }
    }

    public static abstract class ControlView extends ControlResponse {
        public ControlView() {
        }

        public ControlView(ControlView other) {
            super(other);
        }
    }

    public static class View extends ControlView {
        protected String viewName;

        public View(String viewName) {
            this.viewName = viewName;
        }

        public View(View other) {
            super(other);
            this.viewName = other.viewName;
        }

        @Override
        public View copy() {
            return new View(this);
        }

        @Override
        public Type type() {
            return Type.VIEW;
        }

        @Override
        public String value() {
            return viewName();
        }

        public String viewName() {
            return viewName;
        }

        /**
         * Sets the view name to render.
         */
        public View viewName(String viewName) {
            this.viewName = viewName;
            return this;
        }

        @Override
        public View responseName(String responseName) {
            return (View) super.responseName(responseName);
        }

        @Override
        public View error(boolean error) {
            return (View) super.error(error);
        }

        @Override
        public View saveLastView(Boolean saveLastView) {
            return (View) super.saveLastView(saveLastView);
        }

        @Override
        public View saveCurrentView(Boolean saveCurrentView) {
            return (View) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public View saveHomeView(Boolean saveHomeView) {
            return (View) super.saveHomeView(saveHomeView);
        }

        @Override
        public View allowViewSave(Boolean allowViewSave) {
            return (View) super.allowViewSave(allowViewSave);
        }
    }

    public static class ViewLast extends ControlView {
        protected String viewName;
        protected boolean noParam;

        public ViewLast(String viewName) {
            this.viewName = viewName;
        }

        public ViewLast(ViewLast other) {
            super(other);
            this.viewName = other.viewName;
            this.noParam = other.noParam;
        }

        @Override
        public ViewLast copy() {
            return new ViewLast(this);
        }

        @Override
        public Type type() {
            return noParam() ? Type.VIEW_LAST_NOPARAM : Type.VIEW_LAST;
        }

        @Override
        public String value() {
            return viewName();
        }

        public String viewName() {
            return viewName;
        }

        /**
         * Sets the default view name to render when there is no last view.
         */
        public ViewLast viewName(String viewName) {
            this.viewName = viewName;
            return this;
        }

        public boolean noParam() {
            return noParam;
        }

        /**
         * If true, indicates to avoid automatically adding request parameters to the view-last.
         */
        public ViewLast noParam(boolean noParam) {
            this.noParam = noParam;
            return this;
        }

        @Override
        public ViewLast responseName(String responseName) {
            return (ViewLast) super.responseName(responseName);
        }

        @Override
        public ViewLast error(boolean error) {
            return (ViewLast) super.error(error);
        }

        @Override
        public ViewLast saveLastView(Boolean saveLastView) {
            return (ViewLast) super.saveLastView(saveLastView);
        }

        @Override
        public ViewLast saveCurrentView(Boolean saveCurrentView) {
            return (ViewLast) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public ViewLast saveHomeView(Boolean saveHomeView) {
            return (ViewLast) super.saveHomeView(saveHomeView);
        }

        @Override
        public ViewLast allowViewSave(Boolean allowViewSave) {
            return (ViewLast) super.allowViewSave(allowViewSave);
        }
    }

    public static class ViewHome extends ControlView {
        protected String viewName;

        public ViewHome(String viewName) {
            this.viewName = viewName;
        }

        public ViewHome(ViewHome other) {
            super(other);
            this.viewName = other.viewName;
        }

        @Override
        public ViewHome copy() {
            return new ViewHome(this);
        }

        @Override
        public Type type() {
            return Type.VIEW_HOME;
        }

        @Override
        public String value() {
            return viewName();
        }

        public String viewName() {
            return viewName;
        }

        /**
         * Sets the default view name to render when there is no home view.
         */
        public ViewHome viewName(String viewName) {
            this.viewName = viewName;
            return this;
        }

        @Override
        public ViewHome responseName(String responseName) {
            return (ViewHome) super.responseName(responseName);
        }

        @Override
        public ViewHome error(boolean error) {
            return (ViewHome) super.error(error);
        }

        @Override
        public ViewHome saveLastView(Boolean saveLastView) {
            return (ViewHome) super.saveLastView(saveLastView);
        }

        @Override
        public ViewHome saveCurrentView(Boolean saveCurrentView) {
            return (ViewHome) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public ViewHome saveHomeView(Boolean saveHomeView) {
            return (ViewHome) super.saveHomeView(saveHomeView);
        }

        @Override
        public ViewHome allowViewSave(Boolean allowViewSave) {
            return (ViewHome) super.allowViewSave(allowViewSave);
        }
    }

    public static abstract class ControlRequest extends ControlResponse {
        public ControlRequest() {
        }

        public ControlRequest(ControlRequest other) {
            super(other);
        }
    }

    public static class Request extends ControlRequest {
        protected String uri;

        public Request(String uri) {
            this.uri = uri;
        }

        public Request(Request other) {
            super(other);
            this.uri = other.uri;
        }

        @Override
        public Request copy() {
            return new Request(this);
        }

        @Override
        public Type type() {
            return Type.REQUEST;
        }

        @Override
        public String value() {
            return uri();
        }

        public String uri() {
            return uri;
        }

        /**
         * Sets the uri (name) of the request to which to forward.
         */
        public Request uri(String uri) {
            this.uri = uri;
            return this;
        }

        @Override
        public Request responseName(String responseName) {
            return (Request) super.responseName(responseName);
        }

        @Override
        public Request error(boolean error) {
            return (Request) super.error(error);
        }

        @Override
        public Request saveLastView(Boolean saveLastView) {
            return (Request) super.saveLastView(saveLastView);
        }

        @Override
        public Request saveCurrentView(Boolean saveCurrentView) {
            return (Request) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public Request saveHomeView(Boolean saveHomeView) {
            return (Request) super.saveHomeView(saveHomeView);
        }

        @Override
        public Request allowViewSave(Boolean allowViewSave) {
            return (Request) super.allowViewSave(allowViewSave);
        }
    }

    public static abstract class ControlRedirect extends ControlResponse {
        public ControlRedirect() {
            super();
        }

        public ControlRedirect(ControlRedirect other) {
            super(other);
        }

        public Integer statusCode() {
            return statusCode;
        }

        public ControlRedirect statusCode(Integer statusCode) {
            this.statusCode = statusCode;
            return this;
        }

        public Map<String, String> redirectParameterMap() {
            return redirectParameterMap;
        }

        public ControlRedirect redirectParameterMap(Map<String, String> redirectParameterMap) {
            this.redirectParameterMap = redirectParameterMap;
            return this;
        }

        public Map<String, String> redirectParameterValueMap() {
            return redirectParameterValueMap;
        }

        public ControlRedirect redirectParameterValueMap(Map<String, String> redirectParameterValueMap) {
            this.redirectParameterValueMap = redirectParameterValueMap;
            return this;
        }

        public Set<String> excludeParameterSet() {
            return excludeParameterSet;
        }

        public ControlRedirect excludeParameterSet(Set<String> excludeParameterSet) {
            this.excludeParameterSet = excludeParameterSet;
            return this;
        }

        public String includeMode() {
            return includeMode;
        }

        public ControlRedirect includeMode(String includeMode) {
            this.includeMode = includeMode;
            return this;
        }

        public String saveRequest() {
            return saveRequest;
        }

        public ControlRedirect saveRequest(String saveRequest) {
            this.saveRequest = saveRequest;
            return this;
        }

        public Set<String> includeRequestAttributes() {
            return includeRequestAttributes;
        }

        public ControlRedirect includeRequestAttributes(Set<String> includeRequestAttributes) {
            this.includeRequestAttributes = includeRequestAttributes;
            return this;
        }

        public Set<String> excludeRequestAttributes() {
            return excludeRequestAttributes;
        }

        public ControlRedirect excludeRequestAttributes(Set<String> excludeRequestAttributes) {
            this.excludeRequestAttributes = excludeRequestAttributes;
            return this;
        }

        public String connectionState() {
            return connectionState;
        }

        public ControlRedirect connectionState(String connectionState) {
            this.connectionState = connectionState;
            return this;
        }

        public Boolean allowCacheRedirect() {
            return allowCacheRedirect;
        }

        public ControlRedirect allowCacheRedirect(Boolean allowCacheRedirect) {
            this.allowCacheRedirect = allowCacheRedirect;
            return this;
        }
    }

    public static class RequestRedirect extends ControlRedirect {
        protected String uri;
        protected boolean noParam;

        public RequestRedirect(String uri) {
            this.uri = uri;
        }

        public RequestRedirect(RequestRedirect other) {
            super(other);
            this.uri = other.uri;
            this.noParam = other.noParam;
        }

        @Override
        public RequestRedirect copy() {
            return new RequestRedirect(this);
        }

        @Override
        public Type type() {
            return noParam() ? Type.REQUEST_REDIRECT_NOPARAM : Type.REQUEST_REDIRECT;
        }

        @Override
        public String value() {
            return uri();
        }

        public String uri() {
            return uri;
        }

        /**
         * Sets the uri (name) of the request to which to redirect.
         */
        public RequestRedirect uri(String uri) {
            this.uri = uri;
            return this;
        }

        public boolean noParam() {
            return noParam;
        }

        /**
         * If true, indicates to avoid automatically adding request parameters to the redirect.
         */
        public RequestRedirect noParam(boolean noParam) {
            this.noParam = noParam;
            return this;
        }

        @Override
        public RequestRedirect statusCode(Integer statusCode) {
            return (RequestRedirect) super.statusCode(statusCode);
        }

        @Override
        public RequestRedirect responseName(String responseName) {
            return (RequestRedirect) super.responseName(responseName);
        }

        @Override
        public RequestRedirect error(boolean error) {
            return (RequestRedirect) super.error(error);
        }

        @Override
        public RequestRedirect saveLastView(Boolean saveLastView) {
            return (RequestRedirect) super.saveLastView(saveLastView);
        }

        @Override
        public RequestRedirect saveCurrentView(Boolean saveCurrentView) {
            return (RequestRedirect) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public RequestRedirect saveHomeView(Boolean saveHomeView) {
            return (RequestRedirect) super.saveHomeView(saveHomeView);
        }

        @Override
        public RequestRedirect allowViewSave(Boolean allowViewSave) {
            return (RequestRedirect) super.allowViewSave(allowViewSave);
        }

        @Override
        public RequestRedirect redirectParameterMap(Map<String, String> redirectParameterMap) {
            return (RequestRedirect) super.redirectParameterMap(redirectParameterMap);
        }

        @Override
        public RequestRedirect redirectParameterValueMap(Map<String, String> redirectParameterValueMap) {
            return (RequestRedirect) super.redirectParameterValueMap(redirectParameterValueMap);
        }

        @Override
        public RequestRedirect excludeParameterSet(Set<String> excludeParameterSet) {
            return (RequestRedirect) super.excludeParameterSet(excludeParameterSet);
        }

        @Override
        public RequestRedirect includeMode(String includeMode) {
            return (RequestRedirect) super.includeMode(includeMode);
        }

        @Override
        public RequestRedirect saveRequest(String saveRequest) {
            return (RequestRedirect) super.saveRequest(saveRequest);
        }

        @Override
        public RequestRedirect includeRequestAttributes(Set<String> includeRequestAttributes) {
            return (RequestRedirect) super.includeRequestAttributes(includeRequestAttributes);
        }

        @Override
        public RequestRedirect excludeRequestAttributes(Set<String> excludeRequestAttributes) {
            return (RequestRedirect) super.excludeRequestAttributes(excludeRequestAttributes);
        }

        @Override
        public RequestRedirect connectionState(String connectionState) {
            return (RequestRedirect) super.connectionState(connectionState);
        }

        @Override
        public RequestRedirect allowCacheRedirect(Boolean allowCacheRedirect) {
            return (RequestRedirect) super.allowCacheRedirect(allowCacheRedirect);
        }
    }

    public static class RequestRedirectLast extends ControlRedirect {
        protected String uri;

        public RequestRedirectLast(String uri) {
            this.uri = uri;
        }

        public RequestRedirectLast(RequestRedirectLast other) {
            super(other);
            this.uri = other.uri;
        }

        @Override
        public RequestRedirectLast copy() {
            return new RequestRedirectLast(this);
        }

        @Override
        public Type type() {
            return Type.REQUEST_REDIRECT_LAST;
        }

        @Override
        public String value() {
            return uri();
        }

        public String uri() {
            return uri;
        }

        /**
         * Sets default uri (name) of fallback request to which to redirect to if there is no last request.
         */
        public RequestRedirectLast uri(String uri) {
            this.uri = uri;
            return this;
        }

        @Override
        public RequestRedirectLast statusCode(Integer statusCode) {
            return (RequestRedirectLast) super.statusCode(statusCode);
        }

        @Override
        public RequestRedirectLast responseName(String responseName) {
            return (RequestRedirectLast) super.responseName(responseName);
        }

        @Override
        public RequestRedirectLast error(boolean error) {
            return (RequestRedirectLast) super.error(error);
        }

        @Override
        public RequestRedirectLast saveLastView(Boolean saveLastView) {
            return (RequestRedirectLast) super.saveLastView(saveLastView);
        }

        @Override
        public RequestRedirectLast saveCurrentView(Boolean saveCurrentView) {
            return (RequestRedirectLast) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public RequestRedirectLast saveHomeView(Boolean saveHomeView) {
            return (RequestRedirectLast) super.saveHomeView(saveHomeView);
        }

        @Override
        public RequestRedirectLast allowViewSave(Boolean allowViewSave) {
            return (RequestRedirectLast) super.allowViewSave(allowViewSave);
        }

        @Override
        public RequestRedirectLast redirectParameterMap(Map<String, String> redirectParameterMap) {
            return (RequestRedirectLast) super.redirectParameterMap(redirectParameterMap);
        }

        @Override
        public RequestRedirectLast redirectParameterValueMap(Map<String, String> redirectParameterValueMap) {
            return (RequestRedirectLast) super.redirectParameterValueMap(redirectParameterValueMap);
        }

        @Override
        public RequestRedirectLast excludeParameterSet(Set<String> excludeParameterSet) {
            return (RequestRedirectLast) super.excludeParameterSet(excludeParameterSet);
        }

        @Override
        public RequestRedirectLast includeMode(String includeMode) {
            return (RequestRedirectLast) super.includeMode(includeMode);
        }

        @Override
        public RequestRedirectLast saveRequest(String saveRequest) {
            return (RequestRedirectLast) super.saveRequest(saveRequest);
        }

        @Override
        public RequestRedirectLast includeRequestAttributes(Set<String> includeRequestAttributes) {
            return (RequestRedirectLast) super.includeRequestAttributes(includeRequestAttributes);
        }

        @Override
        public RequestRedirectLast excludeRequestAttributes(Set<String> excludeRequestAttributes) {
            return (RequestRedirectLast) super.excludeRequestAttributes(excludeRequestAttributes);
        }

        @Override
        public RequestRedirectLast connectionState(String connectionState) {
            return (RequestRedirectLast) super.connectionState(connectionState);
        }

        @Override
        public RequestRedirectLast allowCacheRedirect(Boolean allowCacheRedirect) {
            return (RequestRedirectLast) super.allowCacheRedirect(allowCacheRedirect);
        }
    }

    public static class CrossRedirect extends ControlRedirect {
        protected String uri;

        public CrossRedirect(String uri) {
            this.uri = uri;
        }

        public CrossRedirect(CrossRedirect other) {
            super(other);
            this.uri = other.uri;
        }

        @Override
        public CrossRedirect copy() {
            return new CrossRedirect(this);
        }

        @Override
        public Type type() {
            return Type.CROSS_REDIRECT;
        }

        @Override
        public String value() {
            return uri();
        }

        public String uri() {
            return uri;
        }

        /**
         * Set the URI, normally from server root (including webapp mount-point and control path if any).
         */
        public CrossRedirect uri(String uri) {
            this.uri = uri;
            return this;
        }

        @Override
        public CrossRedirect statusCode(Integer statusCode) {
            return (CrossRedirect) super.statusCode(statusCode);
        }

        @Override
        public CrossRedirect responseName(String responseName) {
            return (CrossRedirect) super.responseName(responseName);
        }

        @Override
        public CrossRedirect error(boolean error) {
            return (CrossRedirect) super.error(error);
        }

        @Override
        public CrossRedirect saveLastView(Boolean saveLastView) {
            return (CrossRedirect) super.saveLastView(saveLastView);
        }

        @Override
        public CrossRedirect saveCurrentView(Boolean saveCurrentView) {
            return (CrossRedirect) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public CrossRedirect saveHomeView(Boolean saveHomeView) {
            return (CrossRedirect) super.saveHomeView(saveHomeView);
        }

        @Override
        public CrossRedirect allowViewSave(Boolean allowViewSave) {
            return (CrossRedirect) super.allowViewSave(allowViewSave);
        }

        @Override
        public CrossRedirect redirectParameterMap(Map<String, String> redirectParameterMap) {
            return (CrossRedirect) super.redirectParameterMap(redirectParameterMap);
        }

        @Override
        public CrossRedirect redirectParameterValueMap(Map<String, String> redirectParameterValueMap) {
            return (CrossRedirect) super.redirectParameterValueMap(redirectParameterValueMap);
        }

        @Override
        public CrossRedirect excludeParameterSet(Set<String> excludeParameterSet) {
            return (CrossRedirect) super.excludeParameterSet(excludeParameterSet);
        }

        @Override
        public CrossRedirect includeMode(String includeMode) {
            return (CrossRedirect) super.includeMode(includeMode);
        }

        @Override
        public CrossRedirect saveRequest(String saveRequest) {
            return (CrossRedirect) super.saveRequest(saveRequest);
        }

        @Override
        public CrossRedirect includeRequestAttributes(Set<String> includeRequestAttributes) {
            return (CrossRedirect) super.includeRequestAttributes(includeRequestAttributes);
        }

        @Override
        public CrossRedirect excludeRequestAttributes(Set<String> excludeRequestAttributes) {
            return (CrossRedirect) super.excludeRequestAttributes(excludeRequestAttributes);
        }

        @Override
        public CrossRedirect connectionState(String connectionState) {
            return (CrossRedirect) super.connectionState(connectionState);
        }

        @Override
        public CrossRedirect allowCacheRedirect(Boolean allowCacheRedirect) {
            return (CrossRedirect) super.allowCacheRedirect(allowCacheRedirect);
        }
    }

    public static class UrlRedirect extends ControlRedirect {
        protected String url;

        public UrlRedirect(String url) {
            this.url = url;
        }

        public UrlRedirect(UrlRedirect other) {
            super(other);
            this.url = other.url;
        }

        @Override
        public UrlRedirect copy() {
            return new UrlRedirect(this);
        }

        @Override
        public Type type() {
            return Type.URL;
        }

        @Override
        public String value() {
            return url();
        }

        public String url() {
            return url;
        }

        /**
         * The URL to which to redirect, normally absolute.
         */
        public UrlRedirect url(String url) {
            this.url = url;
            return this;
        }

        @Override
        public UrlRedirect statusCode(Integer statusCode) {
            return (UrlRedirect) super.statusCode(statusCode);
        }

        @Override
        public UrlRedirect responseName(String responseName) {
            return (UrlRedirect) super.responseName(responseName);
        }

        @Override
        public UrlRedirect error(boolean error) {
            return (UrlRedirect) super.error(error);
        }

        @Override
        public UrlRedirect saveLastView(Boolean saveLastView) {
            return (UrlRedirect) super.saveLastView(saveLastView);
        }

        @Override
        public UrlRedirect saveCurrentView(Boolean saveCurrentView) {
            return (UrlRedirect) super.saveCurrentView(saveCurrentView);
        }

        @Override
        public UrlRedirect saveHomeView(Boolean saveHomeView) {
            return (UrlRedirect) super.saveHomeView(saveHomeView);
        }

        @Override
        public UrlRedirect allowViewSave(Boolean allowViewSave) {
            return (UrlRedirect) super.allowViewSave(allowViewSave);
        }

        @Override
        public UrlRedirect redirectParameterMap(Map<String, String> redirectParameterMap) {
            return (UrlRedirect) super.redirectParameterMap(redirectParameterMap);
        }

        @Override
        public UrlRedirect redirectParameterValueMap(Map<String, String> redirectParameterValueMap) {
            return (UrlRedirect) super.redirectParameterValueMap(redirectParameterValueMap);
        }

        @Override
        public UrlRedirect excludeParameterSet(Set<String> excludeParameterSet) {
            return (UrlRedirect) super.excludeParameterSet(excludeParameterSet);
        }

        @Override
        public UrlRedirect includeMode(String includeMode) {
            return (UrlRedirect) super.includeMode(includeMode);
        }

        @Override
        public UrlRedirect saveRequest(String saveRequest) {
            return (UrlRedirect) super.saveRequest(saveRequest);
        }

        @Override
        public UrlRedirect includeRequestAttributes(Set<String> includeRequestAttributes) {
            return (UrlRedirect) super.includeRequestAttributes(includeRequestAttributes);
        }

        @Override
        public UrlRedirect excludeRequestAttributes(Set<String> excludeRequestAttributes) {
            return (UrlRedirect) super.excludeRequestAttributes(excludeRequestAttributes);
        }

        @Override
        public UrlRedirect connectionState(String connectionState) {
            return (UrlRedirect) super.connectionState(connectionState);
        }

        @Override
        public UrlRedirect allowCacheRedirect(Boolean allowCacheRedirect) {
            return (UrlRedirect) super.allowCacheRedirect(allowCacheRedirect);
        }
    }
}
