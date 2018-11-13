<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if portlet?has_content>
<table width="100%">
  <tr><td>
    <div>
      <@render resource=portlet.screenLocation name=portlet.screenName reqAttribs={"portalPortletId":portlet.portalPortletId}/>
    </div>
  </td></tr>
</table>
<#else>
<h2>Portlet '${parameters.portalPortletId!}' not found. You may not have the necessary seed or other data for it.</h2>
</#if>
