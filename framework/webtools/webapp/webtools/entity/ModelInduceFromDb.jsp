<%--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
--%>
<%@ page import="java.util.*, java.io.*, java.net.*, java.sql.*, org.ofbiz.base.util.*, org.ofbiz.entity.*, org.ofbiz.entity.model.*, org.ofbiz.entity.datasource.*" %><jsp:useBean id="delegator" type="org.ofbiz.entity.GenericDelegator" scope="request" /><jsp:useBean id="security" type="org.ofbiz.security.Security" scope="request" /><%

if(security.hasPermission("ENTITY_MAINT", session)) {
  String helperName = request.getParameter("helperName");
  if(helperName == null || helperName.length() <= 0) {
    response.setContentType("text/html");
%>

<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html lang="en" dir="ltr" xmlns="http://www.w3.org/1999/xhtml">
<head>
    <title>ModelInduceFromDB</title>
</head>
<body>
<div class='h3'><b>Put the name of the Datasource in the EntityEngine.xml to induce from:</b></div>
<form action='' method="post">
    <input type='text' class='inputBox' size='40' name='helperName' />
    <input type='submit' value='Induce!' />
</form>
It's the datasource name. It doesn't need to be associated with a
 delegator, just defined in entityengine.xml.<br/>
   <b> Use the browser to view the source and see the entities that were created.</b>

</body>
</html>

<%
  } else {
      response.setContentType("text/xml");
      Collection messages = new LinkedList();
      GenericDAO dao = GenericDAO.getGenericDAO(new GenericHelperInfo(null, helperName));
      List newEntList = dao.induceModelFromDb(messages);

      if(messages.size() > 0) {
%>
ERRORS:
<%
        Iterator mIter = messages.iterator();
        while(mIter.hasNext()) {
%>
<%=(String)mIter.next()%><%
        }
      }
      if(newEntList != null) {
        String title = "Entity of the Apache OFBiz Component";
        String description = "None";
        String copyright = "Copyright 2001-2012 The Apache Software Foundation";
        String author = "None";
        String version = "1.0";
%><?xml version="1.0" encoding="UTF-8"?>
<!--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<entitymodel xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:noNamespaceSchemaLocation="http://ofbiz.apache.org/dtds/entitymodel.xsd">
  <!-- ========================================================= -->
  <!-- ======================== Defaults ======================= -->
  <!-- ========================================================= -->
    <title><%=title%></title>
    <description><%=description%></description>
    <copyright><%=copyright%></copyright>
    <author><%=author%></author>
    <version><%=version%></version>

  <!-- ========================================================= -->
  <!-- ======================== Data Model ===================== -->
  <!-- The modules in this file are as follows:                  -->
  <!-- ========================================================= -->

  <!-- ========================================================= -->
  <!-- No Package Name -->
  <!-- ========================================================= -->
<%
  Iterator ecIter = newEntList.iterator();
  while(ecIter.hasNext()) {
    ModelEntity entity = (ModelEntity) ecIter.next();
%>
    <entity entity-name="<%=entity.getEntityName()%>"<%if(!entity.getEntityName().equals(ModelUtil.dbNameToClassName(entity.getPlainTableName())) || !ModelUtil.javaNameToDbName(entity.getEntityName()).equals(entity.getPlainTableName()) ){
          %> table-name="<%=entity.getPlainTableName()%>"<%}%>
            package-name="<%=entity.getPackageName()%>"<%if(entity.getDependentOn().length() > 0){%>
            dependent-on="<%=entity.getDependentOn()%>"<%}%><%if(!title.equals(entity.getTitle())){%>
            title="<%=entity.getTitle()%>"<%}%><%if(!copyright.equals(entity.getCopyright())){%>
            copyright="<%=entity.getCopyright()%>"<%}%><%if(!author.equals(entity.getAuthor())){%>
            author="<%=entity.getAuthor()%>"<%}%><%if(!version.equals(entity.getVersion())){%>
            version="<%=entity.getVersion()%>"<%}%>><%if(!description.equals(entity.getDescription())){%>
      <description><%=entity.getDescription()%></description><%}%><%
  Iterator<ModelField> fieldIterator = entity.getFieldsIterator();
  while (fieldIterator.hasNext()) {
    ModelField field = fieldIterator.next();%>
      <field name="<%=field.getName()%>"<%if(!field.getColName().equals(ModelUtil.javaNameToDbName(field.getName()))){
      %> col-name="<%=field.getColName()%>"<%}%> type="<%=field.getType()%>"><%
    for (String valName : field.getValidators()) {
      %><validate name="<%=valName%>"/><%
    }%></field><%
  }
  Iterator<ModelField> pkIterator = entity.getPksIterator();
  while (pkIterator.hasNext()) {
    ModelField field = pkIterator.next();%>
      <prim-key field="<%=field.getName()%>"/><%
  }
  if (entity.getRelationsSize() > 0) {
    for (int r = 0; r < entity.getRelationsSize(); r++) {
      ModelRelation relation = entity.getRelation(r);%>
      <relation type="<%=relation.getType()%>"<%if(relation.getTitle().length() > 0){%> title="<%=relation.getTitle()%>"<%}
              %> rel-entity-name="<%=relation.getRelEntityName()%>"><%for(ModelKeyMap keyMap : relation.getKeyMaps()){ %>
        <key-map field-name="<%=keyMap.getFieldName()%>"<%if(!keyMap.getFieldName().equals(keyMap.getRelFieldName())){%> rel-field-name="<%=keyMap.getRelFieldName()%>"<%}%> /><%}%>
      </relation><%
    }
  }%>
    </entity><%
  }%>
</entitymodel>
<%
      }
    }
  }
else {
  %>ERROR: You do not have permission to use this page (ENTITY_MAINT needed)<%
}
%>
