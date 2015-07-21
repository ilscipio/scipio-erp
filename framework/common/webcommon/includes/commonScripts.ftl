
<script language="JavaScript" type="text/javascript">
<!-- //

    if (typeof variable === 'undefined') {
        var commonOfbizUrls = {};
    }

    <#-- Common Ofbiz URIs for use in javascript -->
    <@requireScriptOfbizUrl uri="getJSONuiLabelArray" />
    <@requireScriptOfbizUrl uri="getJSONuiLabel" />
    
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

    <#-- theme style variables -->
  <#if styles?has_content>
    var styles = {
    <#list styles?keys as name>
        "${name}" : "${styles[name]!}",
    </#list>
    };
  </#if>  
  

// -->
</script>