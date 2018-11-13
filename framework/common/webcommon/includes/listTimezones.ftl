<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<div class="screenlet">
  <div class="screenlet-title-bar">
    <ul>
      <li class="h3">${uiLabelMap.CommonTimeZone}</li>
      <li><a href="<@ofbizUrl>main</@ofbizUrl>">${uiLabelMap.CommonCancel}</a></li>
    </ul>
    <br class="clear"/>
  </div>
  <table cellspacing="0" class="${styles.table_basic!}"> <#-- orig: class="basic-table hover-bar" -->
    <#assign altRow = true>
    <#assign displayStyle = Static["java.util.TimeZone"].LONG>
    <#assign availableTimeZones = Static["org.ofbiz.base.util.UtilDateTime"].availableTimeZones()/>
    <#list availableTimeZones as availableTz>
      <#assign altRow = !altRow>
      <tr<@tableRowClassAttribStr alt=altRow />>
        <td>
          <a href="<@ofbizUrl>setSessionTimeZone</@ofbizUrl>?tzId=${availableTz.getID()}">${availableTz.getDisplayName(availableTz.useDaylightTime(), displayStyle, locale)} (${availableTz.getID()})</a>
        </td>
      </tr>
    </#list>
  </table>
</div>
