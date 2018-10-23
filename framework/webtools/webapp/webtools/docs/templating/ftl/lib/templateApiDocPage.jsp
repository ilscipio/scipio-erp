<%--
SCIPIO: Templating API Documentation rendering forwarding page 
--%>
<%
request.setAttribute("scipioTmplApiTargetLibPath", request.getPathInfo());
RequestDispatcher rd = request.getRequestDispatcher("/control/ViewTemplateApiDocPage");
rd.forward(request, response);
%>
