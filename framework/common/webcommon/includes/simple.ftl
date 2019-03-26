<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
