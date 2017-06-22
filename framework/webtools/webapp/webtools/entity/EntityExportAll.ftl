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

<p>${uiLabelMap.WebtoolsXMLExportInfo}</p>
<#if results?has_content>
    <@heading>${uiLabelMap.WebtoolsResults}:</@heading>
    <#list results as result>
        <p>${result}</p>
    </#list>
</#if>

<form method="post" action="<@ofbizUrl>entityExportAll</@ofbizUrl>">
    <@field type="input" size="60" name="outpath" value=(outpath!) label=uiLabelMap.WebtoolsOutputDirectory />
    <@field type="datetime" label=uiLabelMap.CommonFromDate name="fromDate" value="" size="25" maxlength="30" id="fromDate" />
    <@field type="input" label=uiLabelMap.WebtoolsTimeoutSeconds size="6" value=txTimeout!'7200' name="txTimeout"/>
    <@field type="submit" text=uiLabelMap.WebtoolsExport class="${styles.link_run_sys!} ${styles.action_export!}" />
</form>
