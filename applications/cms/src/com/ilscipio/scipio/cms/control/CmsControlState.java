package com.ilscipio.scipio.cms.control;

import java.io.Serializable;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.Delegator;

import org.ofbiz.webapp.control.RequestAttrPolicy.RedirectAttrPolicy;
import org.ofbiz.webapp.control.RequestAttrPolicy.ViewLastAttrPolicy;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;

/**
 * Object stored in request that records the current CMS control/render state.
 * <p>
 * Not thread-safe. Although serializable, this is intended for single requests.
 * <p>
 * This class now implements {@link org.ofbiz.webapp.control.RequestAttrPolicy} classes in order to:
 * <ul>
 * <li>Prevent it from entering controller <code>_REQ_ATTR_MAP_</code> completely</li>
 * <li>Thread safety: Anything coming out of the <code>_VIEW_LAST_PARAMS_</code> must be cloned!</li>
 * </ul>
 */
@SuppressWarnings("serial")
public class CmsControlState implements Serializable, RedirectAttrPolicy.NotStorable, ViewLastAttrPolicy.Restorable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final CmsControlState EMPTY = new CmsControlState(); // FIXME: enforce immutable

    public static final String ATTR = "cmsCtrlState";

    private Boolean requestForwarded;
    private Boolean requestChained;

    private String processMappingId;
    private transient CmsProcessMapping processMapping; // will be null if deserialized

    private String requestServletPath;
    private String requestPath;

    private CmsCallType pageRenderMode;

    private String processFullForwardPath;
    private String processExtraPathInfo;
    private String processSourcePath;
    private String processSourcePathMatch;

    private String origRequestUri;
    private String origRequestContextPath;
    private String origRequestSourcePath;
    private String origRequestQueryString;

    private Set<String> controlUris;

    protected CmsControlState() {
    }

    protected CmsControlState(CmsControlState other, Set<String> controlUris) {
        this.requestForwarded = other.requestForwarded;
        this.requestChained = other.requestChained;

        this.processMappingId = other.processMappingId;
        // DON'T, re-look it up in DB to make an copy
        //this.processMapping = other.processMapping; // will be null if deserialized

        this.requestServletPath = other.requestServletPath;
        this.requestPath = other.requestPath;

        this.pageRenderMode = other.pageRenderMode;

        this.processFullForwardPath = other.processFullForwardPath;
        this.processExtraPathInfo = other.processExtraPathInfo;
        this.processSourcePath = other.processSourcePath;
        this.processSourcePathMatch = other.processSourcePathMatch;

        this.origRequestUri = other.origRequestUri;
        this.origRequestContextPath = other.origRequestContextPath;
        this.origRequestSourcePath = other.origRequestSourcePath;
        this.origRequestQueryString = other.origRequestQueryString;

        this.controlUris = controlUris;
    }
    
    /**
     * Gets CMS control state from request, or if does not exist yet, creates it.
     * <p>
     * NOTE: The creation of an empty CmsControlState in request does not affect any processes.
     */
    public static CmsControlState fromRequest(HttpServletRequest request) {
        CmsControlState state = fromRequestIfExists(request);
        if (state == null) {
            state = new CmsControlState();
            request.setAttribute(ATTR, state);
        }
        return state;
    }

    public static CmsControlState fromRequestIfExists(HttpServletRequest request) {
        return (CmsControlState) request.getAttribute(ATTR);
    }

    public static CmsControlState fromRequestOrEmpty(HttpServletRequest request) {
        CmsControlState state = fromRequestIfExists(request);
        return (state != null) ? state : EMPTY;
    }

    public Boolean getRequestForwarded() {
        return requestForwarded;
    }

    public void setRequestForwarded(Boolean requestForwarded) {
        this.requestForwarded = requestForwarded;
    }

    public Boolean getRequestChained() {
        return requestChained;
    }

    public void setRequestChained(Boolean requestChained) {
        this.requestChained = requestChained;
    }

    public CmsProcessMapping getProcessMapping(Delegator delegator, boolean useCache) {
        CmsProcessMapping processMapping = this.getProcessMapping();
        if (processMapping == null) {
            String processMappingId = this.getProcessMappingId();
            if (processMappingId != null) {
                processMapping = CmsProcessMapping.getWorker().findById(delegator, processMappingId, useCache);
                if (processMapping == null) {
                    Debug.logWarning("Cms: Could not find process mapping by ID: " + processMapping + "; ignoring", module);
                } else {
                    setProcessMapping(processMapping);
                }
            }
        }
        return processMapping;
    }

    protected CmsProcessMapping getProcessMapping() {
        return processMapping;
    }

    public void setProcessMapping(CmsProcessMapping processMapping) {
        this.processMapping = processMapping;
        this.processMappingId = (processMapping != null) ? processMapping.getId() : null;
    }

    public String getProcessMappingId() {
        return processMappingId;
    }

    public String getRequestServletPath() {
        return requestServletPath;
    }

    public void setRequestServletPath(String requestServletPath) {
        this.requestServletPath = requestServletPath;
    }

    public String getRequestPath() {
        return requestPath;
    }

    public void setRequestPath(String requestPath) {
        this.requestPath = requestPath;
    }

    public CmsCallType getPageRenderMode() {
        return pageRenderMode;
    }

    public void setPageRenderMode(CmsCallType pageRenderMode) {
        this.pageRenderMode = pageRenderMode;
    }

    public String getProcessFullForwardPath() {
        return processFullForwardPath;
    }

    public void setProcessFullForwardPath(String processFullForwardPath) {
        this.processFullForwardPath = processFullForwardPath;
    }

    public String getProcessExtraPathInfo() {
        return processExtraPathInfo;
    }

    public void setProcessExtraPathInfo(String processExtraPathInfo) {
        this.processExtraPathInfo = processExtraPathInfo;
    }

    public String getProcessSourcePath() {
        return processSourcePath;
    }

    public void setProcessSourcePath(String processSourcePath) {
        this.processSourcePath = processSourcePath;
    }

    public String getProcessSourcePathMatch() {
        return processSourcePathMatch;
    }

    public void setProcessSourcePathMatch(String processSourcePathMatch) {
        this.processSourcePathMatch = processSourcePathMatch;
    }

    public String getOrigRequestUri() {
        return origRequestUri;
    }

    public void setOrigRequestUri(String origRequestUri) {
        this.origRequestUri = origRequestUri;
    }

    public String getOrigRequestContextPath() {
        return origRequestContextPath;
    }

    public void setOrigRequestContextPath(String origRequestContextPath) {
        this.origRequestContextPath = origRequestContextPath;
    }

    public String getOrigRequestSourcePath() {
        return origRequestSourcePath;
    }

    public void setOrigRequestSourcePath(String origRequestSourcePath) {
        this.origRequestSourcePath = origRequestSourcePath;
    }

    public String getOrigRequestQueryString() {
        return origRequestQueryString;
    }

    public void setOrigRequestQueryString(String origRequestQueryString) {
        this.origRequestQueryString = origRequestQueryString;
    }

    protected Set<String> getControlUris() {
        return controlUris;
    }

    protected void setControlUris(Set<String> controlUris) {
        this.controlUris = controlUris;
    }

    @Override
    public String toString() {
        return "[requestForwarded=" + requestForwarded + ", requestChained=" + requestChained
                + ", processMapping=" + processMapping + ", requestServletPath=" + requestServletPath + ", requestPath="
                + requestPath + ", pageRenderMode=" + pageRenderMode + ", processFullForwardPath="
                + processFullForwardPath + ", processExtraPathInfo=" + processExtraPathInfo + ", processSourcePath="
                + processSourcePath + ", processSourcePathMatch=" + processSourcePathMatch + ", origRequestUri="
                + origRequestUri + ", origRequestContextPath=" + origRequestContextPath + ", origRequestSourcePath="
                + origRequestSourcePath + ", origRequestQueryString=" + origRequestQueryString + ", controlUris="
                + controlUris + "]";
    }

    @SuppressWarnings("unchecked")
    @Override
    public Object doViewLastAttrRestore(HttpServletRequest request, String attrName, Map<String, Object> saveAttrMap) {
        // Clone the state, for thread safety
        return new CmsControlState(this, (Set<String>) request.getAttribute("_SCP_FWDROOTURIS_"));
    }
}
