<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<@htmlHeadOpen />
  <@scripts output=true>
    <title>${title!}</title>
    <#-- the trick "<scr" + "ipt below is because browsers should not parse the contents of CDATA elements, but apparently they do. -->
    <@script>
    var jQueryLibLoaded = false;
    function initJQuery() {
        if (typeof(jQuery) == 'undefined') {
            if (!jQueryLibLoaded) {
                jQueryLibLoaded = true;
                document.write("<scr" + "ipt type=\"text/javascript\" src=\"<@contentUrl>/images/jquery/jquery-1.11.0.min.js</@contentUrl>\"></scr" + "ipt>");
                document.write("<scr" + "ipt type=\"text/javascript\" src=\"<@contentUrl>/images/jquery/jquery-migrate-1.2.1.js</@contentUrl>\"></scr" + "ipt>");
            }
            setTimeout("initJQuery()", 50);
        }
    }
    initJQuery();
    </@script>
    <@script src=makeContentUrl("/images/selectall.js") />
    <#if layoutSettings.javaScripts?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = toSet(layoutSettings.javaScripts)/>
        <#list layoutSettings.javaScripts as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <@script src=makeContentUrl(javaScript) />
            </#if>
        </#list>
    </#if>
    <#if layoutSettings.styleSheets?has_content>
        <#list layoutSettings.styleSheets as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${styleSheet}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.VT_STYLESHEET?has_content>
        <#list layoutSettings.VT_STYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${styleSheet}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.VT_HELPSTYLESHEET?has_content && lookupType?has_content>
        <#list layoutSettings.VT_HELPSTYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${styleSheet}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.rtlStyleSheets?has_content && langDir == "rtl">
        <#list layoutSettings.rtlStyleSheets as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${styleSheet}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>
    <#if layoutSettings.VT_RTL_STYLESHEET?has_content && langDir == "rtl">
        <#list layoutSettings.VT_RTL_STYLESHEET as styleSheet>
            <link rel="stylesheet" href="<@contentUrl>${styleSheet}</@contentUrl>" type="text/css"/>
        </#list>
    </#if>

    <@script>
        // This code inserts the value lookedup by a popup window back into the associated form element
        var re_id = new RegExp('id=(\\d+)');
        var num_id = (re_id.exec(String(window.location))
                ? new Number(RegExp.$1) : 0);
        var obj_caller = (window.opener ? window.opener.lookups[num_id] : null);
        if (obj_caller == null)
            obj_caller = window.opener;

        // function passing selected value to calling window
        function set_multivalues(value) {
            obj_caller.target.value = value;
            var thisForm = obj_caller.target.form;
            var evalString = "";

            if (arguments.length > 2 ) {
                for(var i=1; i < arguments.length; i=i+2) {
                    evalString = "setSourceColor(thisForm." + arguments[i] + ")";
                    eval(evalString);
                    evalString = "thisForm." + arguments[i] + ".value='" + arguments[i+1] + "'";
                    eval(evalString);
                }
            }
         }
    </@script>
  </@scripts>
</head>
<body>