
<script language="JavaScript" type="text/javascript">
<!-- //

    if (typeof variable === 'undefined') {
        var commonOfbizUrls = {};
    }

    <#-- Common Ofbiz URIs for use in javascript -->
    <@requireScriptOfbizUrl uri="getJSONuiLabelArray" />
    <@requireScriptOfbizUrl uri="getJSONuiLabel" />
    <#-- This belongs in @progressScript, but Ofbiz FTL bug requires it here -->
    <@requireScriptOfbizUrl uri="getFileUploadProgressStatus" />
    
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
    var catoStyles = {
    <#list styles?keys as name>
        "${name}" : "${styles[name]!}",
    </#list>
    };
  </#if>  
  

// -->
</script>