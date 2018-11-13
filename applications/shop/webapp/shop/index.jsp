<%--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
--%>

<%
String csp = (String) request.getServletContext().getAttribute("_CONTROL_SERVPATH_");
if (csp == null) csp = "";
pageContext.forward(csp + "/main");
%>
