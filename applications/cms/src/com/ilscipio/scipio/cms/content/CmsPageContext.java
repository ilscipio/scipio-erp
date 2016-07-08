package com.ilscipio.scipio.cms.content;

import java.util.Collection;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.control.LoginWorker;
import com.ilscipio.scipio.cms.CmsException;

/**
 * This object encapsulates context information for the rendering of a page.
 * This includes the servlet request, response, and config. It is also used to
 * determine whether a page should be previewed or if the approved version is to
 * be shown.
 * 
 */
public class CmsPageContext {

    public static final String module = CmsPageContext.class.getName();

    private final HttpServletRequest request;
    private final boolean preview;
    private final HttpServletResponse response;
    private final ServletConfig context;
    private final String webSiteId;
    private GenericValue userLogin = null;
    private GenericValue party = null;
    private GenericValue person = null;

    public CmsPageContext(HttpServletRequest request,
            HttpServletResponse response, ServletConfig servletConfig,
            String webSiteId, boolean preview) {
        super();
        this.request = request;
        this.response = response;
        this.context = servletConfig;
        this.webSiteId = webSiteId;
        this.preview = preview;
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

    /**
     * Returns the servlet config which can also be used to obtain the servlet
     * context.
     * 
     * @return context of the CMS servlet
     */
    public ServletConfig getConfig() {
        return context;
    }

    /**
     * Returns the website id used to identify pages and templates.
     * 
     * @return website id
     */
    public String getWebSiteId() {
        return webSiteId;
    }
    
    /**
     * Returns the UserLogin entity of the current user.
     * 
     * @return userLogin entity
     */
    public GenericValue getUserLogin() {
        if (userLogin != null) {
            return userLogin;
        }else if(request.getAttribute("userLogin") != null){
            return (GenericValue) request.getAttribute("userLogin");
        }else if(request.getSession().getAttribute("userLogin") != null){
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
              throw new CmsException("Could not retrieve user login entity from database.", e, module);
        }
        
        return userLogin;
    }

    /**
     * Returns the Person entity of the current user.
     * 
     * @return person entity
     */
    public GenericValue getPerson() {      
        if (person != null) {
            return person;
        }else if(request.getAttribute("person") != null){
            return (GenericValue) request.getAttribute("person");
        }else if(request.getSession().getAttribute("person") != null){
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
            throw new CmsException(
                    "Could not retrieve Person entity.", e, module);
        }
        return person;
    }

    /**
     * Returns the Party entity of the currently logged in user.
     * 
     * @return party entity
     */
    @SuppressWarnings("unchecked")
    public GenericValue getParty() {        
        if (party != null) {
            return party;
        }else if(request.getAttribute("party") != null){
            return (GenericValue) request.getAttribute("party");
        }else if(request.getSession().getAttribute("party") != null){
            return (GenericValue) request.getSession().getAttribute("party");
        }        
        try {
            if (getUserLogin() != null) {
                LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
                Map<String, Object> results = dispatcher.runSync("getPartyFromUserLogin", 
                        UtilMisc.toMap("userLoginId", getUserLogin().getString("userLoginId")));
                
                if (ServiceUtil.isSuccess(results) && results.get("parties") != null) {
                    for (Map<String, GenericValue> i : ((Collection<Map<String, GenericValue>>) results.get("parties"))) {
                        party = i.get("party");
                        break;
                    }
                }
            }
        } catch (GenericServiceException e) {
            throw new CmsException(
                    "Could not retrieve Party entity.", e, module);
        }
        return party;
    }


   


}
