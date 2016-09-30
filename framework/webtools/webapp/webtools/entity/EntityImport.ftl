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

<@section>
<p>${uiLabelMap.WebtoolsXMLImportInfo}</p>
  <form method="post" action="<@ofbizUrl>entityImport</@ofbizUrl>">
    <@field type="input" size="60" name="filename" value=(filename!) label=uiLabelMap.WebtoolsAbsoluteFileNameOrUrl/>
    <@field type="input" size="40" name="fmfilename" value=(fmfilename!) label=uiLabelMap.WebtoolsAbsoluteFTLFilename/>
    <@field type="checkbox" name="isUrl" checked=isUrl?? label=uiLabelMap.WebtoolsIsURL/>
    <@field type="checkbox" name="mostlyInserts" checked=mostlyInserts?? label=uiLabelMap.WebtoolsMostlyInserts/>
    <@field type="checkbox" name="maintainTimeStamps" checked=keepStamps?? label=uiLabelMap.WebtoolsMaintainTimestamps/>
    <@field type="checkbox" name="createDummyFks" checked=createDummyFks?? label=uiLabelMap.WebtoolsCreateDummyFks/>
    <@field type="checkbox" name="checkDataOnly" checked=checkDataOnly?? label=uiLabelMap.WebtoolsCheckDataOnly/>
    <@field type="input" size="6" value=txTimeoutStr!'7200' name="txTimeout" label=uiLabelMap.WebtoolsTimeoutSeconds/>
    <@field type="submit" text=uiLabelMap.WebtoolsImportFile class="+${styles.link_run_sys!} ${styles.action_import!}"/>
  </form>

  <form method="post" action="<@ofbizUrl>entityImport</@ofbizUrl>">
    <@field type="textarea" rows="20" cols="85" name="fulltext" label=uiLabelMap.WebtoolsCompleteXMLDocument>${fulltext!"<entity-engine-xml>\n</entity-engine-xml>"}</@field>
    <@field type="submit" text=uiLabelMap.WebtoolsImportText class="+${styles.link_run_sys!} ${styles.action_import!}"/>
  </form>
  <#if messages??>
    <@section title="${rawString(uiLabelMap.WebtoolsResults)}:">
      <#list messages as message>
          <p>${message}</p>
      </#list>
    </@section>
  </#if>
</@section>