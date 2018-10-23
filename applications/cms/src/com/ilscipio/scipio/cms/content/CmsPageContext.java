package com.ilscipio.scipio.cms.content;

import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.control.LoginWorker;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.cms.CmsException;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;
import com.ilscipio.scipio.cms.template.RendererType;

/**
 * This object encapsulates context information for the rendering of a page.
 * This includes the servlet request, response, and config. It is also used to
 * determine whether a page should be previewed or if the approved version is to
 * be shown.
 * 2017-01-10: This is now also sometimes used to represent the rendering context for
 * non-CMS renderers, where this object simply contains all the variables that are
 * interesting to the CMS code. e.g., this has to be created and
 * passed to make links with PageLinkDirective.makeLinkXxx methods (where it serves as
 * a struct).
 * <p>
 * NOTE: 2016: this object is less important today and largely redundant because all classes that
 * used to rely on this (such as asset load directive) now obtain the
 * MapStack context instead, which has all of the info already.
 * Client code and templates probably should ignore this altogether.
 * <p>
 * TODO: consider renaming this to "CmsContext".
 * <p>
 * NOTE: Not thread-safe (OK for this class).
 */
public class CmsPageContext {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final HttpServletRequest request;
    private final boolean preview;
    private final HttpServletResponse response;
    private final ServletContext servletContext;
    private final String webSiteId;
    private final RendererType rendererType;

    private GenericValue userLogin = null;
    private GenericValue party = null;
    private GenericValue person = null;
    private Delegator delegator = null;

    public CmsPageContext(HttpServletRequest request,
            HttpServletResponse response, ServletContext servletContext,
            String webSiteId, boolean preview, RendererType rendererType) {
        this.request = request;
        this.response = response;
        this.servletContext = servletContext;
        this.webSiteId = webSiteId;
        this.preview = preview;
        this.rendererType = rendererType;
    }

    /**
     * Gets the page context from the context or creates a new generic one if this isn't a CMS render request.
     * Abstracts the way the CmsPageContext is passed and made; it should make one appropriate for the current renderer.
     */
    public static CmsPageContext getOrMakeFromContext(Map<String, ?> context) {
        // for now, if this is a CMS render, there should already be a CmsPageContext created.
        // if it's missing and this is a CMS render, that would be an error, e.g. the template's actions
        // did something stupid and overwrote it.
        // FIXME?: for now if missing must assume is non-cms render... so the generic fallback may actually
        // hide CMS renderer coding errors... nothing can do about this (?) because anything stored in context can be corrupted
        CmsPageContext pageContext = CmsRenderUtil.getPageContext(context);
        if (pageContext == null) {
            pageContext = CmsPageContext.makeFromGenericRequestContext(context);
        }
        return pageContext;
    }

    /**
     * Makes a CmsPageContext from a generic rendering context (non-CMS renderer, usually widget renderer).
     * Must be compatible with {@link org.ofbiz.widget.renderer.ScreenRenderer#populateContextForRequest(HttpServletRequest, HttpServletResponse, ServletContext)}.
     * For webSiteId this uses {@link org.ofbiz.webapp.website.WebSiteWorker#getWebSiteId(ServletRequest)}.
     * NOTE: the CMS renderer context is compatible with this also, but this shouldn't be called for it.
     * WARN: usually you should use {@link #getOrMakeFromContext} instead.
     */
    public static CmsPageContext makeFromGenericRequestContext(Map<String, ?> context) {
        return makeFromRequestContext(context, RendererType.GENERIC);
    }

    /**
     * This is a backwards method that creates a new CmsPageContext from a CMS renderer context,
     * here for completeness.
     * WARN: usually you should use {@link #getOrMakeFromContext} instead.
     */
    public static CmsPageContext makeFromCmsRequestContext(Map<String, ?> context) {
        return makeFromRequestContext(context, RendererType.CMS);
    }

    /**
     * This is a backwards method that creates a new CmsPageContext from a CMS emulated renderer context,
     * here for completeness.
     * WARN: usually you should use {@link #getOrMakeFromContext} instead.
     */
    public static CmsPageContext makeFromCmsEditorRequestContext(Map<String, ?> context) {
        return makeFromRequestContext(context, RendererType.CMS_EDITOR);
    }

    private static CmsPageContext makeFromRequestContext(Map<String, ?> context, RendererType rendererType) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        HttpServletResponse response = (HttpServletResponse) context.get("response");
        ServletContext servletContext = request.getServletContext();
        boolean preview = Boolean.TRUE.equals(context.get("cmsIsPreview"));
        String webSiteId = WebSiteWorker.getWebSiteId(request);
        return new CmsPageContext(request, response, servletContext, webSiteId, preview, rendererType);
    }

    public Delegator getDelegator() {
        if (delegator == null) {
            delegator = (Delegator) request.getAttribute("delegator");
        }
        return delegator;
    }

    /**
     * Returns the request object belonging to the request which triggered the
     * current page.
     *
     * @return request object
     */
    public HttpServletRequest getRequest() {
        return request;
    }

    /**
     * Returns the response object used to render the page.
     *
     * @return response object
     */
    public HttpServletResponse getResponse() {
        return response;
    }

    /**
     * Returns whether the page should be displayed in preview mode.
     *
     * @return if preview mode is active
     */
    public boolean isPreview() {
        return preview;
    }

    // 2016: we should store the ServletContext itself instead
//    /**
//     * Returns the servlet config which can also be used to obtain the servlet
//     * context.
//     *
//     * @return context of the CMS servlet
//     */
//    public ServletConfig getConfig() {
//        return context;
//    }

    /**
     * Returns the servlet context.
     *
     * @return servlet context
     */
    public ServletContext getServletContext() {
        return servletContext;
    }

    /**
     * Returns the website id used to identify pages and templates.
     *
     * @return website id
     */
    public String getWebSiteId() {
        return webSiteId;
    }

    public RendererType getRendererType() {
        return rendererType;
    }

    /**
     * Returns the UserLogin entity of the current user.
     * @deprecated 2016: should not be necessary: can be gotten from
     * regular context ({@link org.ofbiz.widget.renderer.ScreenRenderer#populateContextForRequest})
     *
     * @return userLogin entity
     */
    @Deprecated
    public GenericValue getUserLogin() {
        if (userLogin != null) {
            return userLogin;
        } else if(request.getAttribute("userLogin") != null) {
            return (GenericValue) request.getAttribute("userLogin");
        } else if(request.getSession().getAttribute("userLogin") != null) {
            return (GenericValue) request.getSession().getAttribute("userLogin");
        }

        try {
            Delegator delegator = (Delegator) request.getAttribute("delegator");
            if (this.preview && request.getParameter("previewUser") != null) {
                userLogin = delegator.findOne("UserLogin", true, "userLoginId", request.getParameter("previewUser"));
            }
            if (userLogin == null) {
                LoginWorker.checkExternalLoginKey(request, response);
                userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
            }
            if (userLogin == null) {
                userLogin = delegator.findOne("UserLogin", true, "userLoginId", "anonymous");
            }
        } catch (GenericEntityException e) {
              throw new CmsException("Could not retrieve user login entity from database.", e);
        }

        return userLogin;
    }

    /**
     * Returns the Person entity of the current user.
     * @deprecated 2016: should not be necessary: can be gotten from
     * regular context ({@link org.ofbiz.widget.renderer.ScreenRenderer#populateContextForRequest})
     *
     * @return person entity
     */
    @Deprecated
    public GenericValue getPerson() {
        if (person != null) {
            return person;
        } else if(request.getAttribute("person") != null) {
            return (GenericValue) request.getAttribute("person");
        } else if(request.getSession().getAttribute("person") != null) {
            return (GenericValue) request.getSession().getAttribute("person");
        }
        try {
            if (getParty() != null) {
                LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
                Map<String, Object> result = dispatcher.runSync("getPerson",
                        UtilMisc.toMap("partyId", getParty().getString("partyId")));
                if (ServiceUtil.isSuccess(result)) {
                    person = (GenericValue) result.get("lookupPerson");
                }
            }
        } catch (GenericServiceException e) {
            throw new CmsException("Could not retrieve Person entity.", e);
        }
        return person;
    }

    /**
     * Returns the Party entity of the currently logged in user.
     * @deprecated 2016: should not be necessary: can be gotten from
     * regular context ({@link org.ofbiz.widget.renderer.ScreenRenderer#populateContextForRequest})
     *
     * @return party entity
     */
    @Deprecated
    public GenericValue getParty() {
        if (party != null) {
            return party;
        } else if(request.getAttribute("party") != null) {
            return (GenericValue) request.getAttribute("party");
        } else if(request.getSession().getAttribute("party") != null) {
            return (GenericValue) request.getSession().getAttribute("party");
        }
        try {
            if (getUserLogin() != null) {
                LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
                Map<String, Object> results = dispatcher.runSync("getPartyFromUserLogin",
                        UtilMisc.toMap("userLoginId", getUserLogin().getString("userLoginId")));

                if (ServiceUtil.isSuccess(results) && results.get("parties") != null) {
                    for (Map<String, GenericValue> i : (UtilGenerics.<Map<String, GenericValue>> checkCollection(results.get("parties")))) {
                        party = i.get("party");
                        break;
                    }
                }
            }
        } catch (GenericServiceException e) {
            throw new CmsException("Could not retrieve Party entity.", e);
        }
        return party;
    }

}
