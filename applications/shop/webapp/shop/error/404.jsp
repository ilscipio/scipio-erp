<%--
This file is subject to the terms and conditions defined in
file 'LICENSE', which is part of this source code package.
--%>
<%@ page import="java.util.*" %>
<%@ page import="org.ofbiz.base.util.*" %>
<%@ page import="org.ofbiz.entity.*" %>
<%@ page import="org.ofbiz.entity.util.*" %>
<%@ page import="org.ofbiz.webapp.website.WebSiteWorker" %>
<jsp:useBean id="delegator" type="org.ofbiz.entity.GenericDelegator" scope="request" />
<%
ServletContext context = pageContext.getServletContext();
String webSiteId = WebSiteWorker.getWebSiteId(request);
List<GenericValue> webAnalytics = delegator.findByAnd("WebAnalyticsConfig", UtilMisc.toMap("webSiteId", webSiteId), null, false);
%>
<html>
<head>
<title>Error 404</title>
<%if (webAnalytics != null) {%>
<script language="JavaScript" type="text/javascript">
<%for (GenericValue webAnalytic : webAnalytics) {%>
    <%=StringUtil.wrapString((String) webAnalytic.get("webAnalyticsCode"))%>
<%}%>
</script>
<%}%>
</head>
<body>
<p>
<b>404.</b>
<ins>That&#39;s an error.</ins>
</p>
<p>
The requested URL
<code><%=request.getAttribute("filterRequestUriError")%></code>
was not found on this server.
<ins>That&#39;s all we know.</ins>
</p>
</body>
</html>
