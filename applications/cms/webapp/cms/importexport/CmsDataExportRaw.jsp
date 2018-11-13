<%--
This file is subject to the terms and conditions defined in
file 'LICENSE', which is part of this source code package.
--%>
<%-- 
SCIPIO: based on component://webtools/webapp/webtools/entity/xmldsdumpraw.jsp 
--%>
<%@ page trimDirectiveWhitespaces="true" %>
<%@ page import="
java.io.*, 
java.util.*, 
java.net.*, 
org.w3c.dom.*, 
org.ofbiz.security.*, 
org.ofbiz.entity.*, 
org.ofbiz.entity.condition.*, 
org.ofbiz.entity.util.*,
org.ofbiz.base.util.*, 
org.ofbiz.entity.model.*, 
org.ofbiz.entity.transaction.*, 
com.ilscipio.scipio.cms.data.importexport.CmsDataExportWorker" %>
<jsp:useBean id="security" type="org.ofbiz.security.Security" scope="request" />
<jsp:useBean id="delegator" type="org.ofbiz.entity.GenericDelegator" scope="request" />
<%
  final String module = "xmldsrawdump.jsp";
  if(security.hasPermission("ENTITY_MAINT", session)) {
      CmsDataExportWorker worker = (CmsDataExportWorker) session.getAttribute("cmsDataExportWorker"); // SCIPIO
      // SCIPIO: DO NOT remove this attribute, because it's incredibly annoying to have to restart
      // if you accidentally miss the click.
      // Instead, we will clear this in CmsDataExport.groovy.
      //session.removeAttribute("cmsDataExportWorker");
      if (worker != null && worker.isSingleFile()) {
          worker = worker.cloneWorkerNewState(delegator);
          response.setContentType("text/xml; charset=UTF-8");
          // FIXME:
          //response.setHeader("Content-Disposition","attachment; filename=\"CmsDataExport.xml\"");
          if (worker.hasEntityNames()) {
              try {
                  worker.executeExport(response.getWriter());
              } catch(Exception e) {
                  String errMsg = "CMS Data Export: Error during to-browser export: " + e.getMessage();
                  Debug.logError(e, errMsg, module);
              }
          }
      } else {%>
ERROR: Missing or invalid export configuration found in session, go back to the export page and try again.
    <%}%>
<%} else {%>
ERROR: You do not have permission to use this page (ENTITY_MAINT needed)
<%}%>
