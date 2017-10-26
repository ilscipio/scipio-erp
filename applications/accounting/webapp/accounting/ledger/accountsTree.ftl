<#macro compress_single_line><#local captured><#nested></#local>${captured?replace("^\\s+|\\s+$|\\n|\\r", "", "rm")}</#macro>

<#assign treeOptionsFixedParamsString=""> 
<#if (treeOptionsFixedParams?has_content)>	
	<#assign treeOptionsFixedParamsString=addParamsToStr(treeOptionsFixedParamsString, treeOptionsFixedParams)>
</#if>

<#assign treeOptionsFixedParamsJS>
	<@objectAsScript object=(treeOptionsFixedParams!{}) lang='js'/>
</#assign>

<#-- Javascript functions -->
<@script>
    
    function makeNewPageUrl($node, url, params) {
    	params["parentGlAccountId"] = $node.data["parentGlAccountId"];    	
        var newUrl = url + '?' + $.param(params);
        return newUrl;
    }
    
    function makeEditPageUrl($node, url, params) {
    	params["glAccountId"] = $node.data["glAccountId"];
        var path = $node.data["path"]; //$node.a_attr.href
        var editorUrl = url + '?' + $.param(params);
        return editorUrl;
    }
    
    function makeDeletePageUrl($node, url, params) {
    	params["glAccountId"] = $node.data["glAccountId"];
        var path = $node.data["path"]; //$node.a_attr.href
        var editorUrl = url + '?' + $.param(params);
        return editorUrl;
    } 
    
   function makeAssignGlAccountPageUrl($node, url, params) {   		
   		params["glAccountId"] = $node.data["glAccountId"];
   		params["organizationPartyId"] = $node.data["organizationPartyId"];
        var path = $node.data["path"]; //$node.a_attr.href
        var editorUrl = url + '?' + $.param(params);
        return editorUrl;
    }    
    
    function makeGlAccountTransactionPageUrl($node, url, params) {    	
    	params["glAccountId"] = $node.data["glAccountId"];
   		params["organizationPartyId"] = $node.data["organizationPartyId"];
        var path = $node.data["path"]; //$node.a_attr.href
        var editorUrl = url + '?' + $.param(params);
        return editorUrl;
    }      
    
    function makeImportPageUrl($node, url, params) {
    	var path = $node.data["path"]; //$node.a_attr.href
        var editorUrl = url + '?' + $.param(params);
        return editorUrl;
    } 
                            
    <#-- Function to update the action menu. Will generate new menu items based on selected option -->
    function updateMenu($node, params){
            var $el = $("#action_menu");
    
            var newOptions;
                newOptions = {
                	<#list glAccountUrls.keySet() as glAccountUrlKey>
                		${Static["org.ofbiz.base.util.Debug"].log("glAccountUrlKey ========> " + glAccountUrlKey)}
                		<#if glAccountUrlKey == "createGlAccountUrl">
	                  		"${escapeVal(uiLabelMap.CommonCreate, 'js')}": makeNewPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params)
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "editGlAccountUrl">	                  		
	                  		"${escapeVal(uiLabelMap.CommonEdit, 'js')}": makeEditPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params)
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "deleteGlAccountUrl">
	                  		"${escapeVal(uiLabelMap.CommonDelete, 'js')}": makeDeletePageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params)
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "importGlAccountUrl">
	                  		"${escapeVal(uiLabelMap.CommonImport, 'js')}": makeImportPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params)
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "assignGlAccountUrl">
	                  		"${escapeVal(uiLabelMap.AcctgAssignGlAccount, 'js')}": makeAssignGlAccountPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params)
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "accountTransactionBaseUrl">
	                  		"${escapeVal(uiLabelMap.AccountingAcctgTrans, 'js')}": makeGlAccountTransactionPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params)
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>        		
                	</#list>
                };
            $el.empty(); // remove old options
            $.each(newOptions, function(key,value) {              
              var newEl = $('<@compress_single_line><@menuitem type="link" href="" text=""/></@compress_single_line>');
              var menuAnchor = $(newEl).find('a:last-child');
              menuAnchor.attr("href",value).text(key);
              $el.append(newEl);
            });
    }
    
    function openPageByNode($node){
        document.location.href = makeEditPageUrl($node);
        return false;
    }
    
    <#-- Returns path prefix for the node, but only if not configured to be accounted for
        in the node paths (only if primaryPathFromContextRootDefault is not "Y") -->
    function getCreatePathPrefix($node) {
        <#-- get parent node (the website node) FIXME: couldn't find direct function on $node -->
        var $parentNode = jQuery('#account-tree').jstree().get_node($node.parent);
        
        <#-- retrieve editorRequestPathPrefix if applicable -->
        var createPathPrefix = "";
        if ($node.data.type == "request") {
            if ($parentNode != null && $parentNode.data.type == "website") {
                if ($parentNode.data.primaryPathFromContextRootDefault == "Y") {
                    ;
                } else {
                    createPathPrefix = $parentNode.data.editorRequestPathPrefix;
                }
            }
            if (!createPathPrefix) {
                createPathPrefix = "";
            }
        }
        return createPathPrefix;
    }
</@script>
<#assign treeEvent={'select_node.jstree':'updateMenu(data.node, ' + treeOptionsFixedParamsJS +');','dblclick.jstree"':'openPageByNode(data.node);'}/>
<#assign menuEventScript>
    function($node) {
        var labelCreate = "${escapeVal(uiLabelMap.CommonCreate, 'js')}";
        var labelEdit = "${escapeVal(uiLabelMap.CommonEdit, 'js')}";
        var createDef = {
            "separator_before": false,
            "separator_after": false,
            "label": ($node.data.type=='request') ? labelOverride : labelCreate,
            "action": function (obj) { 
                document.location.href = makeNewPageUrl($node, ${treeOptionsFixedParamsJS});
            }
        };
        var openDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "Open",
            "action":function (obj) {
                document.location.href = makeEditPageUrl($node, ${treeOptionsFixedParamsJS});
            }
        };


        return {
            "Edit": openDef,
            "Create": createDef
        };
    }
</#assign>
<#assign pluginSettings={"items": wrapRawScript(menuEventScript)}/>
<#assign treePlugin =[{"name":"contextmenu", "settings":pluginSettings},{"name":"massload"}]/>

<#-- Content -->
<@section>
    <@row>
            <#-- JSTree displaying all content nodes -->
            <@cell columns=9>
                <@section title=uiLabelMap.AccountingAccounts>
                   
                   <@treemenu type="lib-basic" events=treeEvent plugins=treePlugin id="account-tree"> 
                        <#list accountMaps as account>
                            <#if account.parentGlAccountId?has_content>
                                <@treeitem text="${account.accountCode!} ${account.accountName!}" id=account.glAccountId!  parent=(account.parentGlAccountId!"#") 
                                    attribs={"data":{"type":"${account.glAccountTypeId!}","class":"${account.glAccountClassId!}","code":"${account.accountCode!}","glAccountId":"${account.glAccountId!}","parentGlAccountId":"${account.parentGlAccountId!}","title":"${account.description!}"}} 
                                    icon="${styles.text_color_secondary} ${styles.icon!} ${styles.icon_prefix!}file"/>
                            <#else>
                                <#-- Root item -->
                                <@treeitem text="${account.accountCode!} ${account.accountName!}" id=account.glAccountId!  parent=(account.parentGlAccountId!"#") 
                                    attribs={"data":{"type":"${account.glAccountTypeId!}","class":"${account.glAccountClassId!}","code":"${account.accountCode!}","glAccountId":"${account.glAccountId!}","parentGlAccountId":"${account.parentGlAccountId!}","title":"${account.description!}"}} 
                                    icon="${styles.text_color_secondary} ${styles.icon!} ${styles.icon_prefix!}folder"/>
                            </#if>
                        </#list>    
                    </@treemenu>
                </@section>
            </@cell>
            
            <#-- empty side menu -->
            <@cell columns=3>
                <@section title=uiLabelMap.CommonOptions id="action_offset">
                        <ul class="side-nav" id="action_menu">
                            <@menuitem type="link" href=makeOfbizUrl(glAccountUrls.addGlAccountUrl + '?' + treeOptionsFixedParamsString) text=uiLabelMap.CommonCreate/>
                        </ul>
                </@section>
            </@cell>
            
            <@script>
                $(function() {
                        
                            var $sidebar   = $("#action_offset"), 
                                $window    = $(window),
                                offset     = $sidebar.offset(),
                                topPadding = 50;
                            
                            $window.scroll(function() {
                                if ( $(window).width() > 1024) {
                                    if ($window.scrollTop() > offset.top) {
                                        $sidebar.stop().animate({
                                            marginTop: $window.scrollTop() - $sidebar.position().top + topPadding
                                        });
                                    } else {
                                        $sidebar.stop().animate({
                                            marginTop: 0
                                        });
                                    }
                                }
                            });
                    });
            </@script>
    </@row>
</@section>