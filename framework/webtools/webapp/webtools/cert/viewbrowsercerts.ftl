<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<#assign isSecure = request.isSecure()/>
<#assign clientCerts = request.getAttribute("javax.servlet.request.X509Certificate")!/>
<#if (!clientCerts?has_content)>
    <#assign clientCerts = request.getAttribute("javax.net.ssl.peer_certificates")!/>
</#if>

<@section><#-- SCIPIO: dup: title=uiLabelMap.WebtoolsCertsX509 -->
  <#if (isSecure)>
    <#if (clientCerts?has_content)>
      <@fields type="default-manual">
      <@table type="data-list" class="+${styles.table_spacing_tiny_hint!}"> <#-- orig: class="basic-table" --> <#-- orig: cellspacing="" -->
        <#list clientCerts as cert>
          <#assign certString = Static["org.ofbiz.base.util.KeyStoreUtil"].certToString(cert)!>
          <#if (certString?has_content)>
            <@tr>
              <@td>${uiLabelMap.WebtoolsCertsCert}</@td>
              <@td>${cert.getType()} ${cert.getSubjectX500Principal()}</@td>
            </@tr>
            <@tr>
              <@td>${uiLabelMap.WebtoolsCertsSerialNum}:</@td>
              <@td>${cert.getSerialNumber().toString(16)}</@td>
            </@tr>
            <@tr>
              <@td>&nbsp;</@td>
              <@td>
                <@field type="textarea" rows="4" cols="130">
${certString}

-----BEGIN PUBLIC KEY HEX-----
${Static["org.ofbiz.base.util.KeyStoreUtil"].pemToPkHex(certString)}
-----END PUBLIC KEY HEX-----

                </@field>
              </@td>
            </@tr>
          </#if>
        </#list>
      </@table>
      </@fields>
    <#else>
      <@alert type="info">${uiLabelMap.WebtoolsCertsNotFound}.</@alert>
    </#if>
  <#else>
    <@alert type="info">${uiLabelMap.WebtoolsCertsRequiresSSL}.</@alert>
  </#if>
</@section>