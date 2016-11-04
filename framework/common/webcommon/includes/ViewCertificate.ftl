<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
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
