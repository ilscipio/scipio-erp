
<script language="JavaScript" type="text/javascript">
<!-- //

    if (typeof variable === 'undefined') {
        var commonOfbizUris = {};
    }

    <#-- Common Ofbiz URIs for use in javascript -->
    commonOfbizUris["getJSONuiLabelArray"] = "<@ofbizUrl>getJSONuiLabelArray</@ofbizUrl>";
    commonOfbizUris["getJSONuiLabel"] = "<@ofbizUrl>getJSONuiLabel</@ofbizUrl>";
    
    <#-- TODO: Improve and expand this somehow; too hardcoded! but difficult cause 
         generated URLs depend on controller request defs -->
    function getOfbizUrl(url) {
        if (url in commonOfbizUris) {
            return commonOfbizUris[url];
        }
        else {
            return "";
        }
    }

// -->
</script>