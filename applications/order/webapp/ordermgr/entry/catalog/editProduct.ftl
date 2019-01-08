<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if security.hasEntityPermission("CATALOG", "_CREATE", request) && productId?has_content>
  <@menu type="button">
    <@menuitem type="link" href=makeOfbizInterWebappUrl("/catalog/control/ViewProduct?productId=${rawString(productId)}${rawString(externalKeyParam!)}") target="catalog" text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />
  </@menu>
</#if>
