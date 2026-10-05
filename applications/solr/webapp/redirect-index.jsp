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
<%--
  SCIPIO: 2017-09-13: Custom workaround for Tomcat welcome-file-list issues
  FIXME: should be removed if/when fixed; see web.xml welcome-file-list for details.
--%>
<%
// TODO: REVIEW: forward seemed to work on the surface (after welcome-file fix), 
// but I can't tell if it's safe to forward here... the whole UI comes through this page...
//pageContext.forward("/index.html");
response.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
String location = request.getServletContext().getContextPath() + "/index.html";
String pathInfo = request.getPathInfo(); // may contain #
if (pathInfo != null) {
    location += pathInfo;
}
String queryString = request.getQueryString();
if (queryString == null) {
    queryString = "";
} else if (queryString.length() > 0) {
    queryString = "?" + queryString;
}
response.setHeader("Location", location + queryString);
//response.sendRedirect(request.getServletContext().getContextPath() + "/index.html");
%>