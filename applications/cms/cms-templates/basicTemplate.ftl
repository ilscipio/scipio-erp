<@htmlHeadOpen includeDocType=true />
    <#if pageTitle?has_content><title>${pageTitle}</title></#if>
    <#if customCssFile?has_content>
        <link rel="stylesheet" type="text/css" href="<@contentUrl uri=customCssFile escapeAs='html'/>" media="all" />
    </#if>
    <#if customJsFile?has_content>
        <@script src=makeContentUrl(customJsFile) />
    </#if>
</head>
<body>
    <@asset name="mainContent" />
</body>
</html>
