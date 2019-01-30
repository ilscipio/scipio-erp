<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@script>
function call_fieldlookup3(view_name) {
    var obj_lookupwindow = window.open(view_name + "?webSitePublishPoint=" + webSitePublishPoint,'FieldLookup', 'width=700,height=550,scrollbars=yes,status=no,top='+my+',left='+mx+',dependent=yes,alwaysRaised=yes');
    obj_lookupwindow.opener = window;
    obj_lookupwindow.focus();
}
    function submitRows(rowCount) {
        var rowCountElement = document.createElement("input");
        rowCountElement.setAttribute("name", "_rowCount");
        rowCountElement.setAttribute("type", "hidden");
        rowCountElement.setAttribute("value", rowCount);
        document.forms.siteRoleForm.appendChild(rowCountElement);

        rowCountElement = document.createElement("input");
        rowCountElement.setAttribute("name", "partyId");
        rowCountElement.setAttribute("type", "hidden");
        rowCountElement.setAttribute("value", "${partyId!}");
        document.forms.siteRoleForm.appendChild(rowCountElement);

        rowCountElement = document.createElement("input");
        rowCountElement.setAttribute("name", "userLoginId");
        rowCountElement.setAttribute("type", "hidden");
        rowCountElement.setAttribute("value", "${userLoginId!}");
        document.forms.siteRoleForm.appendChild(rowCountElement);

        rowCountElement = document.createElement("input");
        rowCountElement.setAttribute("name", "webSitePublishPoint");
        rowCountElement.setAttribute("type", "hidden");
        rowCountElement.setAttribute("value", "${webSitePublishPoint!}");
        document.forms.siteRoleForm.appendChild(rowCountElement);

        document.forms.siteRoleForm.submit();
    }

</@script>

<#-- ============================================================= -->

<@section title="WebSitePublishPoint">
  <form name="userform" method="post" action="<@pageUrl>UserPermissions</@pageUrl>">
     <input type="hidden" name="partyId" value="${partyId!}"/>
     <input type="hidden" name="userLoginId" value="${userLoginId!}"/>
     <@field type="input" name="webSitePublishPoint" size="20" value=(webSitePublishPoint!) />
     <@field type="submit" text=uiLabelMap.CommonRefresh class="${styles.link_run_sys!} ${styles.action_reload!}"/>
  </form>
</@section>

<@section>
  <form name="siteRoleForm" method="post" action="<@pageUrl>updateSiteRoles</@pageUrl>">
  <@fields type="default-manual">
      <@table type="data-list" class="+${styles.table_spacing_small_hint!}">
        <@tr>
            <@td>${uiLabelMap.ContentWebSite}</@td>
            <#list blogRoleIdList as roleTypeId>
              <@td>${roleTypeId}</@td>
            </#list>
        </@tr>
      <#assign rowCount=0/>
        <#list siteList as map>
          <@tr>
            <@td>${map.partyId!}</@td>
            <#list blogRoleIdList as roleTypeId>
              <#assign cappedSiteRole= Static["org.ofbiz.entity.model.ModelUtil"].dbNameToVarName(roleTypeId) />
              <@td align="center">
                <@field type="checkbox" inline=true name="${rawString(cappedSiteRole)}_o_${rowCount}" value="Y" checked=(map[cappedSiteRole]?has_content && map[cappedSiteRole] == "Y")/>
                <input type="hidden" name="contentId_o_${rowCount}" value="${webSitePublishPoint}"/>
                <input type="hidden" name="partyId_o_${rowCount}" value="${map.partyId}"/>
              </@td>
            </#list>
          </@tr>
          <#assign rowCount=rowCount + 1/>
        </#list>
        <@tfoot>
          <@tr>
            <@td>
              <@field type="submit" submitType="link" href="javascript:submitRows('${rowCount!}')" class="${styles.link_run_sys!} ${styles.action_update!}" text=uiLabelMap.CommonUpdate />
            </@td>
          </@tr>
        </@tfoot>
      </@table>
  </@fields>
  </form>
</@section>
