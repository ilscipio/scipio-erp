<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#-- SCIPIO: moved to data prep:
<#assign components = Static["org.ofbiz.base.component.ComponentConfig"].getAllComponents()!/>
<#if (requestParameters.certString?has_content)>
    <#assign cert = Static["org.ofbiz.base.util.KeyStoreUtil"].pemToCert(requestParameters.certString)/>
</#if>-->
<@section title=uiLabelMap.CertDetails>
    <#if (cert?has_content)>
      <@row>
        <@cell columns=2 small=2><label>${uiLabelMap.CertType}</label></@cell>
        <@cell columns=10 small=10>${cert.getType()} : ${cert.getSubjectX500Principal()}</@cell>
      </@row>
      <@row>
        <@cell columns=2 small=2><label>${uiLabelMap.CertName}</label></@cell>
        <@cell columns=10 small=10>${cert.getSubjectX500Principal().getName()}</@cell>
      </@row>
      <@row>
        <@cell columns=2 small=2><label>${uiLabelMap.CertSerialNumber}</label></@cell>
        <@cell columns=10 small=10>${cert.getSerialNumber().toString(16)}</@cell>
      </@row>
    <#else>
        <p>${uiLabelMap.CertInvalid}</p>
    </#if>
</@section>
<#if (cert?has_content)>
<@section title=uiLabelMap.CertSaveToKeyStore>
    <@script>
        jQuery(document).ready(function() {
            jQuery('form.savecertform').submit(function(event) {
                var form = jQuery(this);
                var row = form.parents('.savecertform_row').first();
                
                jQuery('input[name=importIssuer]', form).remove();
                var checkboxElem = jQuery('input[name=importIssuerVisible]', row)[0];
                if (checkboxElem.checked) {
                    form.append('<input type="hidden" name="importIssuer" value="' + checkboxElem.value + '" />')
                }
                
                jQuery('input[name=alias]', form).val(jQuery('input[name=aliasVisible]', row).val());
            });
        });
    </@script>
    
    <table cellspacing="0" class="basic-table">
      <thead>
      <tr class="header-row">
            <th>${uiLabelMap.CertComponent}</th>
            <th>${uiLabelMap.CertKeyStore}</th>
            <th>${uiLabelMap.CertImportIssuer}</th>
            <th>${uiLabelMap.CertKeyAlias}</th>
            <th>&nbsp;</th>
      </tr>
      </thead>
      <#list components as component>
        <#assign keystores = component.getKeystoreInfos()!/>
          <#list keystores as store>
            <#if (store.isTrustStore())>
              <tr class="savecertform_row">
                  <td>${component.getComponentName()}</td>
                  <td>${store.getName()}</td>
                  <td><input type="checkbox" name="importIssuerVisible" value="Y"/></td>
                  <td><input type="text" name="aliasVisible" size="20"/></td>
                  <td>
                    <form class="savecertform" method="post" action="<@ofbizUrl>importIssuerProvision</@ofbizUrl>">
                      <input type="hidden" name="componentName" value="${component.getComponentName()}"/>
                      <input type="hidden" name="keystoreName" value="${store.getName()}"/>
                      <input type="hidden" name="certString" value="${parameters.certString}"/>
                      
                      <input type="hidden" name="alias" value=""/>
                      
                      <input type="submit" value="${uiLabelMap.CommonSave}" class="${styles.link_run_sys!} ${styles.action_add!}"/>
                    </form>
                  </td>
              </tr>
            </#if>
          </#list>
      </#list>
    </table>
</@section>
</#if>
