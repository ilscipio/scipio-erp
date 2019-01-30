<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#if security.hasEntityPermission("CATALOG", "_UPDATE", request) && productId?has_content><#-- SCIPIO: changed to _UPDATE from _CREATE -->
  <@menu type="button">
    <@menuitem type="link" href=makeServerUrl("/catalog/control/ViewProduct?productId=${rawString(productId)}${rawString(externalKeyParam!)}")
      target="catalog" text=uiLabelMap.ProductEditProduct class="+${styles.action_nav!} ${styles.action_update!}" />
  </@menu>
</#if>
