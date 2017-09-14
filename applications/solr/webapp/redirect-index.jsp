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