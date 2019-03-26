<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
