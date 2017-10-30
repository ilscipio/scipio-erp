<#macro compress_single_line><#local captured><#nested></#local>${captured?replace("^\\s+|\\s+$|\\n|\\r", "", "rm")}</#macro>

<#assign treeOptionsFixedParamsString=""> 
<#if (treeOptionsFixedParams?has_content)>	
	<#assign treeOptionsFixedParamsString=addParamsToStr(treeOptionsFixedParamsString, treeOptionsFixedParams)>
</#if>

<#assign treeOptionsFixedParamsJS>
	<@objectAsScript object=(treeOptionsFixedParams!{}) lang='js'/>
</#assign>

<#if !ectTreeId?has_content>
  <#assign ectTreeId = "ectTree_" + rawString(topGlAccountId!)>
</#if>
<#assign ectActionProps = toSimpleMap(ectActionProps!{})>

<#assign ectDialogIdPrefix = ectDialogIdPrefix!"ect-dialog-">
<#assign ectDialogIdModalPrefix = ectDialogIdModalPrefix!("modal_"+ectDialogIdPrefix)>

<#-- Javascript functions -->
<@script>


	<#if !ectEmptyMenuItemMarkup?has_content>
      <#assign ectEmptyMenuItemMarkup><@compress_single_line><@menuitem type="link" 
        href="" text=""/></@compress_single_line></#assign>
    </#if>
    <#if !ectEmptyMenuItemMarkupDisabled?has_content>
      <#assign ectEmptyMenuItemMarkupDisabled><@compress_single_line><@menuitem type="link" 
        href="" text="" disabled=true/></@compress_single_line></#assign>
    </#if>
    <#if !ectPostMenuItemMarkup??>
      <#assign ectPostMenuItemMarkup><#rt/>
        <#t/><@compress_single_line><@menuitem type="link" href="javascript:void(0);" onClick="ectHandler.execNewCatalog();" text=uiLabelMap.ProductNewCatalog/></@compress_single_line>
        <#t/><@compress_single_line><@menuitem type="link" href="javascript:void(0);" onClick="ectHandler.execAddCatalog();" text=uiLabelMap.ProductAddExistingCatalog/></@compress_single_line>
      </#assign><#lt/>
    </#if>
    <#if !ectDividerMenuItemMarkup??>
      <#assign ectDividerMenuItemMarkup><#rt/>
        <#t/><@compress_single_line><@menuitem type="generic"><hr/></@menuitem></@compress_single_line>
      </#assign><#lt/>
    </#if>
    
    
if (typeof ectHandler === 'undefined') {
    var ectHandler;
}
    ectHandler = new ScpAccountingTreeHandler({
        treeId: "${escapeVal(ectTreeId!, 'js')}",
      
        labels: {
            error: "${escapeVal(uiLabelMap.CommonError, 'js')}",
            errorfromserver: "${escapeVal(rawLabel('PartyServer'), 'js')}", <#-- FIXME -->
            servercommerror: "${escapeVal(uiLabelMap.CommonServerCommunicationError, 'js')}",
            store: "${escapeVal(uiLabelMap.ProductStore, 'js')}",
            catalog: "${escapeVal(uiLabelMap.ProductCatalog, 'js')}",
            category: "${escapeVal(uiLabelMap.ProductCategory, 'js')}",
            product: "${escapeVal(uiLabelMap.ProductProduct, 'js')}",
            edit: "${escapeVal(uiLabelMap.CommonEdit, 'js')}",
            removeassoc: "${escapeVal(uiLabelMap.CommonRemoveAssoc, 'js')}",
            remove: "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            cannotremovehaschild: "${escapeVal(rawLabelNoSubst('CommonCannotDeleteRecordHasChildrenParam'), 'js')}",
            newcatalog: "${escapeVal(uiLabelMap.ProductNewCatalog, 'js')}",
            newcategory: "${escapeVal(uiLabelMap.ProductNewCategory, 'js')}",
            newsubcategory: "${escapeVal(uiLabelMap.ProductNewSubCategory, 'js')}",
            newproduct: "${escapeVal(uiLabelMap.ProductNewProduct, 'js')}",
            addcatalog: "${escapeVal(uiLabelMap.ProductAddExistingCatalog, 'js')}",
            addcategory: "${escapeVal(uiLabelMap.ProductAddExistingCategory, 'js')}",
            addsubcategory: "${escapeVal(uiLabelMap.ProductAddExistingSubCategory, 'js')}",
            addproduct: "${escapeVal(uiLabelMap.ProductAddExistingProduct, 'js')}",
            manage: "${escapeVal(uiLabelMap.CommonManage, 'js')}",
            removefromstore: "${escapeVal(uiLabelMap.ProductRemoveFromStore, 'js')}",
            removefromcatalog: "${escapeVal(uiLabelMap.ProductRemoveFromCatalog, 'js')}",
            removefromcategory: "${escapeVal(uiLabelMap.ProductRemoveFromCategory, 'js')}",
            deletecatalog: "${escapeVal(uiLabelMap.ProductDeleteCatalog, 'js')}",
            deletecategory: "${escapeVal(uiLabelMap.ProductDeleteCategory, 'js')}",
            deleteproduct: "${escapeVal(uiLabelMap.ProductDeleteProduct, 'js')}",
            editcatalog: "${escapeVal(uiLabelMap.ProductEditCatalog, 'js')}",
            editcategory: "${escapeVal(uiLabelMap.ProductEditCategory, 'js')}",
            editproduct: "${escapeVal(uiLabelMap.ProductEditProduct, 'js')}",
            managecatalog: "${escapeVal(uiLabelMap.ProductManageCatalog, 'js')}",
            managecategory: "${escapeVal(uiLabelMap.ProductManageCategory, 'js')}",
            manageproduct: "${escapeVal(uiLabelMap.ProductManageProduct, 'js')}"
        },
        markup: {
            menuItem: '${ectEmptyMenuItemMarkup}',
            menuItemDisabled: '${ectEmptyMenuItemMarkupDisabled}',
            menuItemDivider: '${ectDividerMenuItemMarkup}',
            postMenuItems: '${ectPostMenuItemMarkup}'
        },
        links: {
            getProductCategoryExtendedData: '<@ofbizUrl uri="getProductCategoryExtendedData" escapeAs="js"/>',
            getProductExtendedData: '<@ofbizUrl uri="getProductExtendedData" escapeAs="js"/>'
        },
        callbacks: <@objectAsScript object=(ectCallbacks!{}) lang='js'/>,
        targetNodeInfo: <@objectAsScript object=(ectTargetNodeInfo!{}) lang='js'/>,
        submittedFormId: "${escapeVal(ectSubmittedFormId!, 'js')}",
        initialParams: <@objectAsScript object=(ectInitialParams!requestParameters!{}) lang='js'/>,
        initialSettings: <@objectAsScript object=(ectInitialSettings!{}) lang='js'/>,
        popupMsgModalId: "${escapeVal(ectPopupMsgModalId!(ectDialogIdModalPrefix+"generic-popupmsg"), 'js')}",
        confirmMsgModalId: "${escapeVal(ectConfirmMsgModalId!(ectDialogIdModalPrefix+"generic-confirmmsg"), 'js')}",
        dialogIdPrefix: "${escapeVal(ectDialogIdModalPrefix, 'js')}"
    });
    
    

	<#assign importGlAccountModal>'<@modal id="importGlAccountModal">Modal Content</@modal>'</#assign>
	var importGlAccountModal = ${escapeScriptString('js', importGlAccountModal)};

	$(document).ready(function() {
		 ectHandler.initBindAll();
		
		/*$('li#importGlAccountUrl a').click(function(e) {
			e.preventDefault();
			$('#modalDialogs').html(importGlAccountModal);			
			openModal($('#importGlAccountModal'));
			console.log("import option clicked");			
		});*/
	});
    
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
	                  		"${escapeVal(uiLabelMap.CommonCreate, 'js')}": [makeNewPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params), '${glAccountUrlKey}']
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "editGlAccountUrl">	                  		
	                  		"${escapeVal(uiLabelMap.CommonEdit, 'js')}": [makeEditPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params), '${glAccountUrlKey}']
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "deleteGlAccountUrl">
	                  		"${escapeVal(uiLabelMap.CommonDelete, 'js')}": [makeDeletePageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params), '${glAccountUrlKey}']
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "importGlAccountUrl">
	                  		"${escapeVal(uiLabelMap.CommonImport, 'js')}": [makeImportPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params), '${glAccountUrlKey}']
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "assignGlAccountUrl">
	                  		"${escapeVal(uiLabelMap.AcctgAssignGlAccount, 'js')}": [makeAssignGlAccountPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params), '${glAccountUrlKey}']
	                  		<#if glAccountUrlKey != glAccountUrls?keys?last>,</#if>
	                  	</#if>
	                  	<#if glAccountUrlKey == "accountTransactionBaseUrl">
	                  		"${escapeVal(uiLabelMap.AccountingAcctgTrans, 'js')}": [makeGlAccountTransactionPageUrl($node, '<@ofbizUrl escapeAs='js'>${glAccountUrls[glAccountUrlKey]}</@ofbizUrl>', params), '${glAccountUrlKey}']
	                  		<#if glAccountUrlKey != glAccountUrls.keySet()?last>,</#if>
	                  	</#if>        		
                	</#list>
                };
            $el.empty(); // remove old options
            $.each(newOptions, function(key,value) {              
              var newEl = $('<@compress_single_line><@menuitem type="link" href="" text=""/></@compress_single_line>');
              var menuAnchor = $(newEl).find('a:last-child');
              menuAnchor.attr("href",value[0]).text(key);
              menuAnchor.attr("id", value[1]);
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


<#assign treeEvents = {
    'select_node.jstree': 'ectHandler.sideMenuHandler(data.node);',
    'activate_node.jstree': 'ectHandler.execEditForNode(data.node);' <#-- TODO: REVIEW: is this too destructive? -->
    <#-- no longer supported by jstree
    'dblclick.jstree': 'ectHandler.execManageForNode(data.node);'-->
}/>

<#assign treePlugins = [
    {"name":"contextmenu", "settings":{
        "items": wrapRawScript("function(node) { return ectHandler.dropMenuHandler(node); }")
    }},
    {"name":"massload"},
    {"name":"dnd", "settings":{
        "is_draggable": wrapRawScript("function(nodeList) { return ectHandler.areNodesDraggable(nodeList); }")
         <#-- "large_drop_target":true  - not helping -->
    }} 
]/>
<#assign treeSettings = {
    "multiple": false, <#-- TODO: in future could implement partial multiple operations (remove/move/copy) -->
    "check_callback": wrapRawScript("function(operation, node, node_parent, node_position, more) { return ectHandler.treeCheckCallback(operation, node, node_parent, node_position, more); }")
} + toSimpleMap(ectTreeSettings!{})>

<#-- Content -->
<@section>
    <@row>
            <#-- JSTree displaying all content nodes -->
            <@cell columns=9>
                <@section title=uiLabelMap.AccountingAccounts>
                   
                   <@treemenu id=ectTreeId settings=treeSettings plugins=treePlugins events=treeEvents>
                   <#-- <@treemenu type="lib-basic" events=treeEvent plugins=treePlugin id="account-tree"> --> 
                        <#list treeMenuData as node>
                            <#if node.isParent!false>                            	 
                            	 <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
		                            attribs={"data":{
		                                "type": node.type!,
		                                "li_attr": node.li_attr!{},
		                                "glAccountEntity": node.glAccountEntity!{},		                                
		                                "isParent": node.isParent!false
		                            }} 
		                            state=(node.state!{})
		                            icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}folder"/>
                            <#else>
                                <#-- Root item -->
                                <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
		                            attribs={"data":{
		                                "type": node.type!,
		                                "li_attr": node.li_attr!{},
		                                "glAccountEntity": node.glAccountEntity!{},		                                
		                                "isParent": node.isParent!false
		                            }} 
		                            state=(node.state!{})
		                            icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}file"/>
                            </#if>
                        </#list>    
                    </@treemenu>
                </@section>
            </@cell>
            
            <#-- empty side menu -->
            <@cell columns=3>
                <@section title=uiLabelMap.CommonOptions id="action_offset">
                        <ul class="side-nav" id="action_menu">
                            <@menuitem type="link" href=makeOfbizUrl(glAccountUrls.addGlAccountUrl + '?' + treeOptionsFixedParamsString) text=uiLabelMap.CommonCreate id="addGlAccountUrl"/>
                            <@menuitem type="link" href=makeOfbizUrl(glAccountUrls.importGlAccountUrl + '?' + treeOptionsFixedParamsString) text=uiLabelMap.CommonImport id="importGlAccountUrl" />
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
<@section id="modalDialogs">
</@section>

