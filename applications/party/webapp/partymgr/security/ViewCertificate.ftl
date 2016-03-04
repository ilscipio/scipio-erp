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
<#-- should be done in screen
<#assign components = Static["org.ofbiz.base.component.ComponentConfig"].getAllComponents()!/>
<#if (requestParameters.certString?has_content)>
    <#assign cert = Static["org.ofbiz.base.util.KeyStoreUtil"].pemToCert(requestParameters.certString)/>
</#if>-->
<@section id="findPartyParameters" title=uiLabelMap.PageTitleImportCertificate>
    <#if (cert?has_content)>
        <span>${uiLabelMap.PartyCertType}</span>&nbsp;${cert.getType()} : ${cert.getSubjectX500Principal()}
        <span>${uiLabelMap.PartyCertName}</span>&nbsp;${cert.getSubjectX500Principal().getName()}
        <span>${uiLabelMap.PartyCertSerialNumber}</span>&nbsp;${cert.getSerialNumber().toString(16)}
    <#else>
        <@commonMsg type="warning">${uiLabelMap.PartyCertInvalid}</@commonMsg>
    </#if>
</@section>
<@section title=uiLabelMap.PartyCertSaveToKeyStore>
  <@table type="data-list"> <#-- orig: cellspacing="0" -->
    <@thead>
      <@tr class="header-row">
        <@th>${uiLabelMap.PartyCertComponent}</@th>
        <@th>${uiLabelMap.PartyCertKeyStore}</@th>
        <@th>${uiLabelMap.PartyCertImportIssuer}</@th>
        <@th>${uiLabelMap.PartyCertKeyAlias}</@th>
        <@th>&nbsp;</@th>
      </@tr>
    </@thead>
    <@tbody>
      <#list components as component>
        <#assign keystores = component.getKeystoreInfos()!/>
          <#list keystores as store>
            <#if (store.isTrustStore())>
              <@tr>
                <form method="post" action="<@ofbizUrl>importIssuerProvision</@ofbizUrl>">
                  <input type="hidden" name="componentName" value="${component.getComponentName()}"/>
                  <input type="hidden" name="keystoreName" value="${store.getName()}"/>
                  <input type="hidden" name="certString" value="${requestParameters.certString}"/>

                  <@td>${component.getComponentName()}</@td>
                  <@td>${store.getName()}</@td>
                  <@td align="center"><input type="checkbox" name="importIssuer" value="Y"/></@td>
                  <@td><input type="text" name="alias" size="20"/></@td>
                  <@td align="right"><input type="submit" value="${uiLabelMap.CommonSave}" class="${styles.link_run_sys!} ${styles.action_add!}"/></@td>
                </form>
              </@tr>
            </#if>
          </#list>
      </#list>
    </@tbody>
  </@table>
</@section>