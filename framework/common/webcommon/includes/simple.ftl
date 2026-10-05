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
<#--
Changes to this file: Copyright (C) Ilscipio GmbH. The changes are licensed
under the GNU Affero General Public License, version 3, or a commercial
license from Ilscipio GmbH (file LICENSE). The original code stays under
the Apache License, version 2.0, as stated above.
-->
<@htmlHeadOpen />
  <@scripts output=true>
    <title>${layoutSettings.companyName!}<#if title?has_content>: ${title}<#elseif titleProperty?has_content>: ${uiLabelMap[titleProperty]}</#if></title>
    <#if layoutSettings.shortcutIcon?has_content>
      <link rel="shortcut icon" href="<@contentUrl>${layoutSettings.shortcutIcon}</@contentUrl>" />
    </#if>
    <#if layoutSettings.javaScripts?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#list layoutSettings.javaScripts as javaScript>
            <@script src=makeContentUrl("${javaScript}") />
        </#list>
    </#if>
    <#if layoutSettings.styleSheets?has_content>
        <#--layoutSettings.styleSheets is a list of style sheets. So, you can have a user-specified "main" style sheet, AND a component style sheet.-->
        <#list layoutSettings.styleSheets as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${styleSheet}</@contentUrl>" type="text/css"/>
        </#list>
    <#else>
        <link rel="stylesheet" href="<@contentUrl>/images/maincss.css</@contentUrl>" type="text/css"/>
    </#if>
    <#if layoutSettings.rtlStyleSheets?has_content && langDir == "rtl">
        <#--layoutSettings.rtlStyleSheets is a list of rtl style sheets.-->
        <#list layoutSettings.rtlStyleSheets as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${styleSheet}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
  </@scripts>
</head>

<body id="column-container" style="background: white;">
<form name="printPage">
<input type="button" value="${uiLabelMap.CommonPrint}" onclick="window.print()" class="${styles.link_run_sys!} ${styles.action_export!}"/>
</form>
<br />
${sections.render("body")}
</body>
</html>
