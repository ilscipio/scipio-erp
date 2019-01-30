  <@scripts output=true> <#-- ensure @script elems here will always output -->
    <#-- New in scipio; priority footer javascripts (before screen footer javascripts) -->
    <#if layoutSettings.VT_FTPR_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_FTPR_JAVASCRIPT)/>
        <#list layoutSettings.VT_FTPR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- New in scipio; for app scripts that aren't (exclusively) styling but must go at end of page -->
    <#if layoutSettings.javaScriptsFooter?has_content>
        <#assign javaScriptsSet = toSet(layoutSettings.javaScriptsFooter)/>
        <#list layoutSettings.javaScriptsFooter as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- For theme styling-related scripts -->
    <#if layoutSettings.VT_FTR_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_FTR_JAVASCRIPT)/>
        <#list layoutSettings.VT_FTR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>

    <#-- New in scipio; always-bottom guaranteed-last javascripts -->
    <#if layoutSettings.VT_BTM_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.VT_BTM_JAVASCRIPT)/>
        <#list layoutSettings.VT_BTM_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
  </@scripts>
</body>
</html>
