<#if !efpTreeId?has_content>
  <#assign efpTreeId = "efpTree_" + rawString(orgPartyId!)>
</#if>
<#assign efpActionProps = toSimpleMap(efpActionProps!{})>

<#assign efpDialogIdPrefix = efpDialogIdPrefix!"efp-dialog-">
<#assign efpDialogIdModalPrefix = efpDialogIdModalPrefix!("modal_"+efpDialogIdPrefix)>

<#-- DEFAULT MARKUP - NOTE: see also treecommon.ftl for additional default markup (localized fields) -->
  <div style="display:none;">

    <#assign efpDefMarkupIdPrefix = efpDefMarkupIdPrefix!"efp-markup-">
    <#assign efpDefMarkupSelectors = {
        "menuItem": "#"+efpDefMarkupIdPrefix+"menuItem",
        "menuItemDisabled": "#"+efpDefMarkupIdPrefix+"menuItemDisabled",
        "menuItemDivider": "#"+efpDefMarkupIdPrefix+"menuItemDivider",
        "postMenuItems": "#"+efpDefMarkupIdPrefix+"postMenuItems"
    }>

    <#-- menu -->
    <#macro efpDefMarkupMenuItem args={}><@menuitem type="link" href="" text=""/></#macro>
    <#macro efpDefMarkupMenuItemDisabled args={}><@menuitem type="link" href="" text="" disabled=true/></#macro>
    <#macro efpDefMarkupMenuItemDivider args={}><@menuitem type="generic"><hr/></@menuitem></#macro>
    <#macro efpDefMarkupPostMenuItems args={}>
    	<#--  <@menuitem type="link" href="javascript:void(0);" onClick="efpHandler.execNewGlAccount();" text=uiLabelMap.CommonCreate /> -->
        <#-- <@menuitem type="link" href="javascript:void(0);" onClick="efpHandler.execImport();" text=uiLabelMap.CommonImport /> -->   
        <@menuitem type="link" href="javascript:void(0);" onClick="efpHandler.execNewTimePeriod();" text=uiLabelMap.CommonAdd/><#t/>     
    </#macro>
    <#macro efpDefMarkupMenu args={}>
        <@menu class="side-nav acctg-action-menu box" id="acctg-action-menu-timePeriod">
          <@acctgMarkupOut dir=efpMarkupPostMenuItems!efpDefMarkupPostMenuItems/>
        </@menu>        
    </#macro>

    <#-- PRE-OUTPUTTED MARKUP FOR JAVASCRIPT 
        DEV NOTE: got rid of @compress_single_line because fail on complex markup, must store as html instead. -->
    
    <#macro efpDefMarkupEntry name>
      <div id="${efpDefMarkupIdPrefix}${name}"><#nested></div>
    </#macro>
    <@efpDefMarkupEntry name="menuItem"><@acctgMarkupOut dir=efpMarkupMenuItem!efpDefMarkupMenuItem/></@efpDefMarkupEntry>
    <@efpDefMarkupEntry name="menuItemDisabled"><@acctgMarkupOut dir=efpMarkupMenuItemDisabled!efpDefMarkupMenuItemDisabled/></@efpDefMarkupEntry>
    <@efpDefMarkupEntry name="menuItemDivider"><@acctgMarkupOut dir=efpMarkupMenuItemDivider!efpDefMarkupMenuItemDivider/></@efpDefMarkupEntry>
    <@efpDefMarkupEntry name="postMenuItems"><@acctgMarkupOut dir=efpMarkupPostMenuItems!efpDefMarkupPostMenuItems/></@efpDefMarkupEntry>
  </div>
  
  <#-- modals -->
  
  
<#-- Javascript functions -->
<@script>    
	if (typeof efptHandler === 'undefined') {
	    var efpHandler;
	}
	efpHandler = new ScpAccountingTreeHandler({
	    treeId: "${escapeVal(efpTreeId!, 'js')}",	    
	    actionProps: <@objectAsScript object=(efpActionProps!{}) lang='js'/>,
	    hideShowFormIds: <@objectAsScript object=(efpAllHideShowFormIds![]) lang='js'/>,
	    labels: {
	        error: "${escapeVal(uiLabelMap.CommonError, 'js')}",
	        errorfromserver: "${escapeVal(rawLabel('PartyServer'), 'js')}", <#-- FIXME -->
	        servercommerror: "${escapeVal(uiLabelMap.CommonServerCommunicationError, 'js')}",            
	        edit: "${escapeVal(uiLabelMap.CommonEdit, 'js')}",       
	        remove: "${escapeVal(uiLabelMap.CommonRemove, 'js')}",                  
	        add: "${escapeVal(uiLabelMap.CommonAdd, 'js')}",
	        create: "${escapeVal(uiLabelMap.CommonCreate, 'js')}",           
	        manage: "${escapeVal(uiLabelMap.CommonManage, 'js')}"           
	    },
	    markupSelectors: <@objectAsScript object=efpDefMarkupSelectors lang='js'/>,
	    links: {           
	    	getTimePeriodExtendedData: '<@ofbizUrl uri="getTimePeriodExtendedData" escapeAs="js"/>',
	    },
	    callbacks: <@objectAsScript object=(efpCallbacks!{}) lang='js'/>,
	    targetNodeInfo: <@objectAsScript object=(efpTargetNodeInfo!{}) lang='js'/>,
	    submittedFormId: "${escapeVal(efpSubmittedFormId!, 'js')}",
	    initialParams: <@objectAsScript object=(efpInitialParams!requestParameters!{}) lang='js'/>,
	    initialSettings: <@objectAsScript object=(efpInitialSettings!{}) lang='js'/>,
	    popupMsgModalId: "${escapeVal(efpPopupMsgModalId!(efpDialogIdModalPrefix+"generic-popupmsg"), 'js')}",
	    confirmMsgModalId: "${escapeVal(efpConfirmMsgModalId!(efpDialogIdModalPrefix+"generic-confirmmsg"), 'js')}",
	    dialogIdPrefix: "${escapeVal(efpDialogIdModalPrefix, 'js')}",
	    objectLocFields: <@objectAsScript object=(efpObjectLocalizedFields!{}) lang='js' />
	});
	
	$(document).ready(function() {
		 efpHandler.initBindAll();
	});
</@script> 
  
<#assign treeEvents = {
    'select_node.jstree': 'efpHandler.sideMenuHandler(data.node);',
    'activate_node.jstree': 'efpHandler.execEditForNode(data.node);' <#-- TODO: REVIEW: is this too destructive? -->
    <#-- no longer supported by jstree
    'dblclick.jstree': 'efpHandler.execManageForNode(data.node);'-->
}/>
<#assign treePlugins = [
    {"name":"contextmenu", "settings":{
        "items": wrapRawScript("function(node) { return efpHandler.dropMenuHandler(node); }")
    }},
    {"name":"massload"},
    {"name":"dnd", "settings":{
        "is_draggable": wrapRawScript("function(nodeList) { return efpHandler.areNodesDraggable(nodeList); }")
         <#-- "large_drop_target":true  - not helping -->
    }} 
]/>
<#assign treeSettings = {
    "multiple": false, <#-- TODO: in future could implement partial multiple operations (remove/move/copy) -->
    "check_callback": wrapRawScript("function(operation, node, node_parent, node_position, more) { return efpHandler.treeCheckCallback(operation, node, node_parent, node_position, more); }")
} + toSimpleMap(efpTreeSettings!{})>
	
<@section id="mainSection" >
	<@row>
	    <@cell medium=9 large=9>	
	    	<@section title=uiLabelMap.AccountingTimePeriod>
	    		<@treemenu id=efpTreeId settings=treeSettings plugins=treePlugins events=treeEvents>
	                <#list treeMenuData as node>
	                    <#if node.isParent!false>                            	 
	                    	 <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
	                            attribs={"data":{
	                                "type": node.type!,
	                                "li_attr": node.li_attr!{},
	                                "customTimePeriodEntity": node.customTimePeriodEntity!{},		                                
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
	                                "customTimePeriodEntity": node.customTimePeriodEntity!{},		                                
	                                "isParent": node.isParent!false
	                            }} 
	                            state=(node.state!{})
	                            icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}file"/>
	                    </#if>
	                </#list>    
	            </@treemenu>
			</@section>
			
     		<#-- POST-JSTREE (ACTION FORMS, ETC.) -->
      		<@acctgMarkupOut dir=efpPostTreeArea!/>
		</@cell>
		<@cell medium=3 large=3>    
	      <#-- ACTIONS MENU -->
	      <@section title=uiLabelMap.CommonActions>
	      	<@efpDefMarkupMenu/>
	      	<#-- 
	      	<ul class="side-nav acctg-action-menu" id="acctg-action-menu"">		       	
	        	<li>
	        		<@menuitem contentId="efp-timePeriod-add" class="+efp-menu-action" type="link" href="javascript:void(0);" text=uiLabelMap.CommonAdd />
	        	</li>
	        </ul>
	        -->
	      </@section>
	   </@cell>	       
	</@row>			
</@section>	
	