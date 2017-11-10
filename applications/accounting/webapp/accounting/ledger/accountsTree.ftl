<#if !egltTreeId?has_content>
  <#assign egltTreeId = "egltTree_" + rawString(topGlAccountId!)>
</#if>
<#assign egltActionProps = toSimpleMap(egltActionProps!{})>

<#assign egltDialogIdPrefix = egltDialogIdPrefix!"eglt-dialog-">
<#assign egltDialogIdModalPrefix = egltDialogIdModalPrefix!("modal_"+egltDialogIdPrefix)>

<#-- DEFAULT MARKUP - NOTE: see also treecommon.ftl for additional default markup (localized fields) -->
  <div style="display:none;">

    <#assign egltDefMarkupIdPrefix = egltDefMarkupIdPrefix!"eglt-markup-">
    <#assign egltDefMarkupSelectors = {
        "menuItem": "#"+egltDefMarkupIdPrefix+"menuItem",
        "menuItemDisabled": "#"+egltDefMarkupIdPrefix+"menuItemDisabled",
        "menuItemDivider": "#"+egltDefMarkupIdPrefix+"menuItemDivider",
        "postMenuItems": "#"+egltDefMarkupIdPrefix+"postMenuItems"
    }>

    <#-- menu -->
    <#macro egltDefMarkupMenuItem args={}><@menuitem type="link" href="" text=""/></#macro>
    <#macro egltDefMarkupMenuItemDisabled args={}><@menuitem type="link" href="" text="" disabled=true/></#macro>
    <#macro egltDefMarkupMenuItemDivider args={}><@menuitem type="generic"><hr/></@menuitem></#macro>
    <#macro egltDefMarkupPostMenuItems args={}>
    	<#--  <@menuitem type="link" href="javascript:void(0);" onClick="egltHandler.execNewGlAccount();" text=uiLabelMap.CommonCreate /> -->
        <@menuitem type="link" href="javascript:void(0);" onClick="egltHandler.execImport();" text=uiLabelMap.CommonImport />
    </#macro>
    <#macro egltDefMarkupMenu args={}>
        <ul class="side-nav eglt-action-menu" id="eglt-action-menu">
          <@egltMarkupOut dir=egltMarkupPostMenuItems!egltDefMarkupPostMenuItems/>
        </ul>
    </#macro>

    <#-- PRE-OUTPUTTED MARKUP FOR JAVASCRIPT 
        DEV NOTE: got rid of @compress_single_line because fail on complex markup, must store as html instead. -->
    
    <#macro egltDefMarkupEntry name>
      <div id="${egltDefMarkupIdPrefix}${name}"><#nested></div>
    </#macro>
    <@egltDefMarkupEntry name="menuItem"><@egltMarkupOut dir=egltMarkupMenuItem!egltDefMarkupMenuItem/></@egltDefMarkupEntry>
    <@egltDefMarkupEntry name="menuItemDisabled"><@egltMarkupOut dir=egltMarkupMenuItemDisabled!egltDefMarkupMenuItemDisabled/></@egltDefMarkupEntry>
    <@egltDefMarkupEntry name="menuItemDivider"><@egltMarkupOut dir=egltMarkupMenuItemDivider!egltDefMarkupMenuItemDivider/></@egltDefMarkupEntry>
    <@egltDefMarkupEntry name="postMenuItems"><@egltMarkupOut dir=egltMarkupPostMenuItems!egltDefMarkupPostMenuItems/></@egltDefMarkupEntry>
  </div>
    
    <#-- modals -->
    <#macro egltDefActionConfirmMsgBtn args={}>
        <div class="modal-footer ${styles.text_right!}">
            <#-- NOTE: the value "continue" is extracted from the class and passed to the callback -->
            <a class="eglt-dialogbtn eglt-dialogbtn-continue ${styles.button!} btn-ok">${uiLabelMap.CommonContinue}</a>
        </div>
    </#macro>
    
    <#macro egltDefActionMsgModals args={}>
        <#list args.actionProps?keys as objectType>
            <#local actionMap = toSimpleMap(args.actionProps[objectType])>
    
            <#-- DEV NOTE: supports <form class="ect-dialogopts-form"><input...>...</form> inside the modal for extra options -->
            <#local props = actionMap["remove"]!{}>
            <#if props.confirmMsg?has_content>
                <@modal id="${args.idPrefix}${rawString(objectType)}-remove" class="+eglt-dialogmodal">
                    <@heading>${uiLabelMap.CommonWarning}</@heading>
                    <div class="eglt-dialogmsg"></div>
                    <@egltDefActionConfirmMsgBtn/>
                </@modal>
            </#if>    
        </#list>
     
      <#if !egltPopupMsgModalId?has_content>
        <@modal id="${args.idPrefix}generic-popupmsg" class="+eglt-dialogmodal">
            <@heading>${uiLabelMap.CommonWarning}</@heading>
            <div class="eglt-dialogmsg"></div>
        </@modal>
      </#if>
      <#if !egltConfirmMsgModalId?has_content>
        <@modal id="${args.idPrefix}generic-confirmmsg" class="+eglt-dialogmodal">
            <@heading>${uiLabelMap.CommonWarning}</@heading>
            <div class="eglt-dialogmsg"></div>
            <@egltDefActionConfirmMsgBtn/>
        </@modal>
      </#if>
    </#macro>

    <#-- OUTPUT MODALS (not in display:none;) -->
    <@egltMarkupOut dir=(egltActionMsgModals!egltDefActionMsgModals) args={
        "actionProps": egltActionProps!{},
        "idPrefix": egltDialogIdPrefix,
        "idModalPrefix": egltDialogIdModalPrefix
    }/>

<#-- Javascript functions -->
<@script>
    
if (typeof egltHandler === 'undefined') {
    var egltHandler;
}
egltHandler = new ScpAccountingTreeHandler({
    treeId: "${escapeVal(egltTreeId!, 'js')}",
    topGlAccountId: <@objectAsScript object=(topGlAccountId!) lang='js'/>,
  	topGlAccountEntity: <@objectAsScript object=(topGlAccount!) lang='js'/>,
    actionProps: <@objectAsScript object=(egltActionProps!{}) lang='js'/>,
    hideShowFormIds: <@objectAsScript object=(egltAllHideShowFormIds![]) lang='js'/>,
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
    markupSelectors: <@objectAsScript object=egltDefMarkupSelectors lang='js'/>,
    links: {           
    	getGlAccountExtendedData: '<@ofbizUrl uri="getGlAccountExtendedData" escapeAs="js"/>',
    },
    callbacks: <@objectAsScript object=(egltCallbacks!{}) lang='js'/>,
    targetNodeInfo: <@objectAsScript object=(egltTargetNodeInfo!{}) lang='js'/>,
    submittedFormId: "${escapeVal(egltSubmittedFormId!, 'js')}",
    initialParams: <@objectAsScript object=(egltInitialParams!requestParameters!{}) lang='js'/>,
    initialSettings: <@objectAsScript object=(egltInitialSettings!{}) lang='js'/>,
    popupMsgModalId: "${escapeVal(egltPopupMsgModalId!(egltDialogIdModalPrefix+"generic-popupmsg"), 'js')}",
    confirmMsgModalId: "${escapeVal(egltConfirmMsgModalId!(egltDialogIdModalPrefix+"generic-confirmmsg"), 'js')}",
    dialogIdPrefix: "${escapeVal(egltDialogIdModalPrefix, 'js')}",
    objectLocFields: <@objectAsScript object=(egltObjectLocalizedFields!{}) lang='js' />
});

$(document).ready(function() {
	 egltHandler.initBindAll();
});
</@script>

<#assign treeEvents = {
    'select_node.jstree': 'egltHandler.sideMenuHandler(data.node);',
    'activate_node.jstree': 'egltHandler.execEditForNode(data.node);' <#-- TODO: REVIEW: is this too destructive? -->
    <#-- no longer supported by jstree
    'dblclick.jstree': 'egltHandler.execManageForNode(data.node);'-->
}/>
<#assign treePlugins = [
    {"name":"contextmenu", "settings":{
        "items": wrapRawScript("function(node) { return egltHandler.dropMenuHandler(node); }")
    }},
    {"name":"massload"},
    {"name":"dnd", "settings":{
        "is_draggable": wrapRawScript("function(nodeList) { return egltHandler.areNodesDraggable(nodeList); }")
         <#-- "large_drop_target":true  - not helping -->
    }} 
]/>
<#assign treeSettings = {
    "multiple": false, <#-- TODO: in future could implement partial multiple operations (remove/move/copy) -->
    "check_callback": wrapRawScript("function(operation, node, node_parent, node_position, more) { return egltHandler.treeCheckCallback(operation, node, node_parent, node_position, more); }")
} + toSimpleMap(egltTreeSettings!{})>

<#-- Content -->
<@fieldset collapsed=false collapsible=true title="Show GL Accounts tree">
<@row>
    <#-- JSTree displaying all content nodes -->
    <@cell medium=9 large=9>
        <@section title=uiLabelMap.AccountingAccounts>
           <@treemenu id=egltTreeId settings=treeSettings plugins=treePlugins events=treeEvents>
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
        
        <#-- POST-JSTREE (ACTION FORMS, ETC.) -->
      <@egltMarkupOut dir=egltPostTreeArea!/>
        
    </@cell>
    
    <@cell medium=3 large=3>    
      <#-- ACTIONS MENU -->
      <@section title=uiLabelMap.CommonActions>	        
        <#-- MENU -->
        <@egltMarkupOut dir=egltMarkupMenu!egltDefMarkupMenu/>	        
        <#-- DISPLAY EXTRAS (OPTIONS, ETC.) -->
        <@egltMarkupOut dir=egltActionExtrasArea!/>	
      </@section>	      
      <#-- DISPLAY EXTRAS (OPTIONS, ETC.) -->
      <@egltMarkupOut dir=egltExtrasArea!/>      
	</@cell>
</@row>
</@fieldset>


