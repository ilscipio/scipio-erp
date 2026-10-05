<#--
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
-->

<html>
<head>
  <title>${uiLabelMap.EcommerceTellAFriend}</title>
</head>
<body class="ecbody">
    <form name="tellafriend" action="<@pageUrl>emailFriend</@pageUrl>" method="post">
        <#if (requestParameters.productId)?? || (requestParameters.productId)??>
            <input type="hidden" name="pageUrl" value="<@catalogAltUrl fullPath=true productCategoryId=requestParameters.categoryId!"" productId=requestParameters.productId!""/>" />
        <#else>
            <#assign cancel = "Y">
        </#if>
        <input type="hidden" name="webSiteId" value="${context.webSiteId!}"/>
      <#if !cancel??>
        <@table type="fields">
          <@tr>
            <@td>${uiLabelMap.CommonYouremail}:</@td>
            <@td><input type="text" name="sendFrom" size="30" /></@td>
          </@tr>
          <@tr>
            <@td>${uiLabelMap.CommonEmailTo}:</@td>
            <@td><input type="text" name="sendTo" size="30" /></@td>
          </@tr>
          <@tr>
            <@td colspan="2" align="center">${uiLabelMap.CommonMessage}</@td>
          </@tr>
          <@tr>
            <@td colspan="2" align="center">
              <textarea cols="40"  rows="5" name="message"></textarea>
            </@td>
          </@tr>
          <@tr>
            <@td colspan="2" align="center">
              <input type="submit" value="${uiLabelMap.CommonSend}" class="${styles.link_run_sys!} ${styles.action_send!}" />
            </@td>
          </@tr>
        </@table>
      <#else>
        <@script>
          window.close();
        </@script>
        <div>${uiLabelMap.EcommerceTellAFriendSorry}</div>
      </#if>
    </form>
</body>
</html>
