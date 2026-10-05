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
<%@ page import="org.ofbiz.base.util.*" %>
<html>
<head>
<title>SCIPIO</title>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
</head>

<% String errorMsg = (String) request.getAttribute("_ERROR_MESSAGE_"); %>

<body bgcolor="#FFFFFF">
<div align="center">
  <br/>
  <table width="100%" border="1" height="200">
    <tr>
      <td>
        <table width="100%" border="0" height="200">
          <tr bgcolor="#CC6666">
            <td height="45">
              <div align="center"><font face="Verdana, Arial, Helvetica, sans-serif" size="4" color="#FFFFFF"><b>:ERROR MESSAGE:</b></font></div>
            </td>
          </tr>
          <tr>
            <td>
              <div align="left"><font face="Verdana, Arial, Helvetica, sans-serif" size="2"><%=UtilFormatOut.replaceString(errorMsg, "\n", "<br/>")%></font></div>
            </td>
          </tr>
        </table>
      </td>
    </tr>
  </table>
</div>
<div align="center"></div>
</body>
</html>