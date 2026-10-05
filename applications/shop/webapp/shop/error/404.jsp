<%--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
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
