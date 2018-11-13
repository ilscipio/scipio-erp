<%--
This file is subject to the terms and conditions defined in
file 'LICENSE', which is part of this source code package.
--%>

<%
String csp = (String) request.getServletContext().getAttribute("_CONTROL_SERVPATH_");
if (csp == null) csp = "";
pageContext.forward(csp + "/main");
%>
