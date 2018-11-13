<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<#if security.hasEntityPermission("CATALOG", "_CREATE", session)>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizInterWebappUrl("/catalog/control/ViewProduct?productId=${productId}${externalKeyParam!}") target="catalog" text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />
  </@menu>
</#if>
