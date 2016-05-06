
<#-- TODO: could remove output=true later and let accumulate in theme footer -->
<@script output=true>

    <#-- Common Ofbiz URIs for use in javascript -->
    <@requireScriptOfbizUrl uri="getJSONuiLabelArray" onlyIfExists=true/>
    <@requireScriptOfbizUrl uri="getJSONuiLabel" onlyIfExists=true/>
    <#-- This belongs in @progressScript, but Ofbiz FTL bug requires it here -->
    <@requireScriptOfbizUrl uri="getFileUploadProgressStatus" onlyIfExists=true/>
    
    <#-- NOTE: a screen that needs a URL in JS must call @requireScriptOfbizUrl 
         FTL macro, for now, see htmlUtilities.ftl -->
    function getOfbizUrl(url) {
        if (url in commonOfbizUrls) {
            return commonOfbizUrls[url];
        }
        else {
            return "";
        }
    }

    <#-- theme style variables 
         TODO?: could be optimized via static JS generated manually or cached -->
  <#if styles?has_content>
    var catoStyles = <@objectAsScript lang="js" object=styles />;
  </#if>  
  

</@script>