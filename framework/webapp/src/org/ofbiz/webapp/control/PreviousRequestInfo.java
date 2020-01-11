package org.ofbiz.webapp.control;

import org.ofbiz.base.util.UtilValidate;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;
import java.util.Collections;
import java.util.Map;

/**
 * SCIPIO: Small class for the _PREVIOUS_REQUEST_INFO_ session attribute, to replace the problematic _PREVIOUS_REQUEST_
 * and related attributes and fix the problem of synchronization between the several attributes (badly handled in stock ofbiz).
 * <p>
 * The {@link #setInfo} calls set the previous legacy session attributes in case any templates use them directly,
 * but it is highly recommended to use the static accessors instead.
 * <p>
 * TODO: Check if the _REQ_ATTR_MAP_ should somehow be integrated into this...
 * </p>
 * <p>
 * TODO?: Factory for future client extensions (based on webSiteId or factory in request attributes), seems unnecessary for now.
 */
public class PreviousRequestInfo {
    public static final String ATTR = "_PREVIOUS_REQUEST_INFO_";
    private static final PreviousRequestInfo DEFAULT_UNSET = new PreviousRequestInfo(null, null,  null, null, Collections.emptyMap());

    protected final String previousRequest; // _PREVIOUS_REQUEST_
    protected final String previousServletRequest; // _PREVIOUS_SERVLET_REQUEST_ (Scipio extension)
    protected final Map<String, Object> previousParamMapUrl; // _PREVIOUS_PARAM_MAP_URL_
    protected final Map<String, Object> previousParamMapForm; // _PREVIOUS_PARAM_MAP_FORM_
    protected final Map<String, Object> attributes; // extra read-only attributes (to modify, make copy), for client code to carry info around (instead of factory, for now)

    protected PreviousRequestInfo(String previousRequest, String previousServletRequest, Map<String, Object> previousParamMapUrl, Map<String, Object> previousParamMapForm, Map<String, Object> attributes) {
        this.previousRequest = previousRequest;
        this.previousServletRequest = previousServletRequest;
        this.previousParamMapUrl = previousParamMapUrl;
        this.previousParamMapForm = previousParamMapForm;
        this.attributes = attributes;
    }

    public static PreviousRequestInfo getInfo(HttpServletRequest request) {
        HttpSession session = request.getSession(false);
        return (session == null) ? null : (PreviousRequestInfo) session.getAttribute(ATTR);
    }

    public static PreviousRequestInfo getInfoOrUnset(HttpServletRequest request) {
        PreviousRequestInfo prevReqInfo = getInfo(request);
        return (prevReqInfo != null) ? prevReqInfo : getUnset(request);
    }

    public static PreviousRequestInfo getUnset(HttpServletRequest request) {
        return DEFAULT_UNSET;
    }

    public static PreviousRequestInfo createInfo(HttpServletRequest request, String previousRequest, String previousServletRequest, Map<String, Object> previousParamMapUrl, Map<String, Object> previousParamMapForm, Map<String, Object> attributes) {
        return new PreviousRequestInfo(previousRequest, previousServletRequest,
                UtilValidate.isNotEmpty(previousParamMapUrl) ? Collections.unmodifiableMap(previousParamMapUrl) : null,
                UtilValidate.isNotEmpty(previousParamMapForm) ? Collections.unmodifiableMap(previousParamMapForm) : null,
                UtilValidate.isNotEmpty(attributes) ? Collections.unmodifiableMap(attributes) : Collections.emptyMap());
    }

    public static void setInfo(HttpServletRequest request, PreviousRequestInfo prevReqInfo) {
        setInfo(request, (prevReqInfo != null) ? prevReqInfo : getUnset(request));
    }

    public void setInfo(HttpServletRequest request) {
        HttpSession session;
        if (isSet()) {
            session = request.getSession();
            session.setAttribute(ATTR, this);
        } else {
            session = request.getSession(false);
            if (session != null) {
                session.setAttribute(ATTR, null); // NOTE: same as removeAttribute (per java servlet spec)
            }
        }
        if (session != null) {
            session.setAttribute("_PREVIOUS_REQUEST_", getPreviousRequest());
            //session.setAttribute("_PREVIOUS_SERVLET_REQUEST_", getPreviousServletRequest()); // unneeded (scipio extension)
            session.setAttribute("_PREVIOUS_PARAM_MAP_URL_", getPreviousParamMapUrl());
            session.setAttribute("_PREVIOUS_PARAM_MAP_FORM_", getPreviousParamMapForm());
        }
    }

    public static void removeInfo(HttpServletRequest request) {
        getUnset(request).setInfo(request);
    }

    public boolean isSet() {
        return getPreviousRequest() != null || getPreviousServletRequest() != null || getPreviousParamMapUrl() != null || getPreviousParamMapForm() != null || !getAttributes().isEmpty();
    }

    public String getPreviousRequest() { return previousRequest; }
    public String getPreviousServletRequest() { return previousServletRequest; }
    public Map<String, Object> getPreviousParamMapUrl() { return previousParamMapUrl; }
    public Map<String, Object> getPreviousParamMapForm() { return previousParamMapForm; }
    public Map<String, Object> getAttributes() { return attributes; }

    public static String getPreviousRequest(HttpServletRequest request) { return getInfoOrUnset(request).getPreviousRequest(); }
    public static String getPreviousServletRequest(HttpServletRequest request) { return getInfoOrUnset(request).getPreviousServletRequest(); }
    public static Map<String, Object> getPreviousParamMapUrl(HttpServletRequest request) { return getInfoOrUnset(request).getPreviousParamMapUrl(); }
    public static Map<String, Object> getPreviousParamMapForm(HttpServletRequest request) { return getInfoOrUnset(request).getPreviousParamMapForm(); }

    /**
     * Returns copy of this that has the requests removed but leaves the _PREVIOUS_PARAM_MAP_URL_ and _PREVIOUS_PARAM_MAP_FORM_ intact.
     * Needed for RequestHandler logic.
     */
    public PreviousRequestInfo removePreviousRequest() {
        return new PreviousRequestInfo(null, null, getPreviousParamMapUrl(), getPreviousParamMapForm(), getAttributes());
    }
}
