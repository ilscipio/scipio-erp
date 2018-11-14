<%--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
--%>
<%@ page contentType="text/plain" %><%@ page import="java.util.*" %><%@ page import="org.ofbiz.entity.*" %><%@ page import="org.ofbiz.entity.model.*" %><jsp:useBean id="delegator" type="org.ofbiz.entity.GenericDelegator" scope="request" /><jsp:useBean id="security" type="org.ofbiz.security.Security" scope="request" /><%
if(security.hasPermission("ENTITY_MAINT", session)) {
  ModelReader reader = delegator.getModelReader();
  Collection<String> ec = reader.getEntityNames();
  TreeSet<String> entities = new TreeSet<String>(ec);
  Iterator<String> classNamesIterator = entities.iterator();
  while(classNamesIterator != null && classNamesIterator.hasNext()) { ModelEntity entity = reader.getModelEntity((String)classNamesIterator.next());%>
CREATE TABLE <%=entity.getPlainTableName()%> (<%for (String fieldName : entity.getAllFieldNames()){ModelField field=entity.getField(fieldName); ModelFieldType type = delegator.getEntityFieldType(entity, field.getType());%><%if(field.getIsPk()){%>
  <%=field.getColName()%> <%=type.getSqlType()%> NOT NULL,<%}else{%>
  <%=field.getColName()%> <%=type.getSqlType()%>,<%}%><%}%>
  CONSTRAINT PK_<%=entity.getPlainTableName()%> PRIMARY KEY (<%=entity.colNameString(entity.getPkFieldsUnmodifiable())%>));
<%}%>
<%
}
else {
  %>ERROR: You do not have permission to use this page (ENTITY_MAINT needed)<%
}
%>
