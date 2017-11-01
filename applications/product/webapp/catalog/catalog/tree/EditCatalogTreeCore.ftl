<#-- SCIPIO: Interactive catalog tree core include
    NOTE: The main javascript is now in ScpEctCommon.js

    FIXME:
    * tree does not support multiple ProdCatalogCategory associations for same
      prodCatalogId & productCategoryId but different prodCatalogCategoryTypeId - it will simply break down.
      The multiple assoc is permitted by schema because prodCatalogCategoryTypeId is part of PK.
      NOTE: The service addProductCategoryCatAssocVersatile tries to prevent this (even though it shouldn't), 
          but it cannot prevent other user-build cases.
    * dialog messages and confirmMsg are not fully escaped (but unescaped parts are internal so not security issue)
-->

<#-- TODO: theme styling -->
<style type="text/css">
    <#-- FIXME: theme doesn't show gray for disabled + priority problems -->
    ul.side-nav.ect-action-menu li.disabled, ul.side-nav.ect-action-menu a.disabled {
        color:gray;
    }
 
    .ect-dialogmsg-recordname {
        font-style:italic;
    }
    
    <#-- FIXME: horrible, and doesn't work when tree not loaded... -->
    div div div.ect-locfield-entry select, div div div.ect-locfield-entry input {
        margin-bottom:0;
    }
    
    div.ect-locfield-add-cnt {
        text-align:right;
    }
</style>

<#if !ectTreeId?has_content>
  <#assign ectTreeId = "ectTree_" + rawString(productStoreId!)>
</#if>
<#assign ectActionProps = toSimpleMap(ectActionProps!{})>

<#assign ectDialogIdPrefix = ectDialogIdPrefix!"ect-dialog-">
<#assign ectDialogIdModalPrefix = ectDialogIdModalPrefix!("modal_"+ectDialogIdPrefix)>

<#-- DEFAULT MARKUP - NOTE: see also treecommon.ftl for additional default markup (localized fields) -->
<#--<div>-->
  <div style="display:none;">

    <#assign ectDefMarkupIdPrefix = ectDefMarkupIdPrefix!"ect-markup-">
    <#assign ectDefMarkupSelectors = {
        "menuItem": "#"+ectDefMarkupIdPrefix+"menuItem",
        "menuItemDisabled": "#"+ectDefMarkupIdPrefix+"menuItemDisabled",
        "menuItemDivider": "#"+ectDefMarkupIdPrefix+"menuItemDivider",
        "postMenuItems": "#"+ectDefMarkupIdPrefix+"postMenuItems"
    }>

    <#-- menu -->
    <#macro ectDefMarkupMenuItem args={}><@menuitem type="link" href="" text=""/></#macro>
    <#macro ectDefMarkupMenuItemDisabled args={}><@menuitem type="link" href="" text="" disabled=true/></#macro>
    <#macro ectDefMarkupMenuItemDivider args={}><@menuitem type="generic"><hr/></@menuitem></#macro>
    <#macro ectDefMarkupPostMenuItems args={}>
        <@menuitem type="link" href="javascript:void(0);" onClick="ectHandler.execNewCatalog();" text=uiLabelMap.ProductNewCatalog/><#t/>
        <@menuitem type="link" href="javascript:void(0);" onClick="ectHandler.execAddCatalog();" text=uiLabelMap.ProductAddExistingCatalog/><#t/>
    </#macro>
    <#macro ectDefMarkupMenu args={}>
        <ul class="side-nav ect-action-menu" id="ect-action-menu">
          <@ectMarkupOut dir=ectMarkupPostMenuItems!ectDefMarkupPostMenuItems/>
        </ul>
    </#macro>

    <#-- PRE-OUTPUTTED MARKUP FOR JAVASCRIPT 
        DEV NOTE: got rid of @compress_single_line because fail on complex markup, must store as html instead. -->
    
    <#macro ectDefMarkupEntry name>
      <div id="${ectDefMarkupIdPrefix}${name}"><#nested></div>
    </#macro>
    <@ectDefMarkupEntry name="menuItem"><@ectMarkupOut dir=ectMarkupMenuItem!ectDefMarkupMenuItem/></@ectDefMarkupEntry>
    <@ectDefMarkupEntry name="menuItemDisabled"><@ectMarkupOut dir=ectMarkupMenuItemDisabled!ectDefMarkupMenuItemDisabled/></@ectDefMarkupEntry>
    <@ectDefMarkupEntry name="menuItemDivider"><@ectMarkupOut dir=ectMarkupMenuItemDivider!ectDefMarkupMenuItemDivider/></@ectDefMarkupEntry>
    <@ectDefMarkupEntry name="postMenuItems"><@ectMarkupOut dir=ectMarkupPostMenuItems!ectDefMarkupPostMenuItems/></@ectDefMarkupEntry>
  </div>
    
    <#-- modals -->
    <#macro ectDefActionConfirmMsgBtn args={}>
        <div class="modal-footer ${styles.text_right!}">
            <#-- NOTE: the value "continue" is extracted from the class and passed to the callback -->
            <a class="ect-dialogbtn ect-dialogbtn-continue ${styles.button!} btn-ok">${uiLabelMap.CommonContinue}</a>
        </div>
    </#macro>
    
    <#macro ectDefActionInnerContent props>
        <p class="ect-dialogmsg"></p>
      <#if props.confirmFields??>
        <@form class="+ect-dialogopts-form">
            <#-- TODO: REVIEW: unclear if the value should be "all" or "active" at current time or if should
                be checked by default -->
          <@fields type="default-compact">
            <@ectMarkupOut dir=props.confirmFields/>
          </@fields>
        </@form>
      </#if>
        <p class="ect-dialogextramsg"></p>
    </#macro>
    
    <#macro ectDefActionMsgModals args={}>
        <#list args.actionProps?keys as objectType>
            <#local actionMap = toSimpleMap(args.actionProps[objectType])>
    
            <#-- DEV NOTE: supports <form class="ect-dialogopts-form"><input...>...</form> inside the modal for extra options -->
            
            <#local props = actionMap["removeassoc"]!{}>
            <#if props.confirmMsg?has_content>
                <@modal id="${args.idPrefix}${rawString(objectType)}-removeassoc" class="+ect-dialogmodal">
                    <@heading>${uiLabelMap.CommonWarning}</@heading>
                    <@ectDefActionInnerContent props=props/>
                    <div class="modal-footer ${styles.text_right!}">
                       <#-- NOTE: the value "remove"/"expire" is extracted from the class and passed to the callback -->
                       <a class="ect-dialogbtn ect-dialogbtn-remove ${styles.button!} btn-ok">${uiLabelMap.CommonRemove}</a>
                       <a class="ect-dialogbtn ect-dialogbtn-expire ${styles.button!} btn-ok">${uiLabelMap.CommonExpire}</a>
                    </div>
                </@modal>
            </#if>
            
            <#local props = actionMap["remove"]!{}>
            <#if props.confirmMsg?has_content>
                <@modal id="${args.idPrefix}${rawString(objectType)}-remove" class="+ect-dialogmodal">
                    <@heading>${uiLabelMap.CommonWarning}</@heading>
                    <@ectDefActionInnerContent props=props/>
                    <@ectDefActionConfirmMsgBtn/>
                </@modal>
            </#if>
        
            <#local props = actionMap["copymoveassoc"]!{}>
            <#if props.confirmMsg?has_content>
                <@modal id="${args.idPrefix}${rawString(objectType)}-copymoveassoc" class="+ect-dialogmodal">
                    <@heading>${uiLabelMap.CommonWarning}</@heading>
                    <@ectDefActionInnerContent props=props/>
                    <div class="modal-footer ${styles.text_right!}">
                       <#-- NOTE: the value "remove"/"expire" is extracted from the class and passed to the callback -->
                       <a class="ect-dialogbtn ect-dialogbtn-copy ${styles.button!} btn-ok">${uiLabelMap.CommonCopy}</a>
                       <a class="ect-dialogbtn ect-dialogbtn-move-remove ${styles.button!} btn-ok">${uiLabelMap.CommonMove}</a><#-- (${uiLabelMap.CommonRemove}) -->
                       <a class="ect-dialogbtn ect-dialogbtn-move-expire ${styles.button!} btn-ok">${uiLabelMap.CommonMove} (${uiLabelMap.CommonExpire})</a>
                    </div>
                </@modal>
            </#if>
        </#list>
     
      <#if !ectPopupMsgModalId?has_content>
        <@modal id="${args.idPrefix}generic-popupmsg" class="+ect-dialogmodal">
            <@heading>${uiLabelMap.CommonWarning}</@heading>
            <@ectDefActionInnerContent props=props/>
        </@modal>
      </#if>
      <#if !ectConfirmMsgModalId?has_content>
        <@modal id="${args.idPrefix}generic-confirmmsg" class="+ect-dialogmodal">
            <@heading>${uiLabelMap.CommonWarning}</@heading>
            <@ectDefActionInnerContent props=props/>
            <@ectDefActionConfirmMsgBtn/>
        </@modal>
      </#if>
    </#macro>

    <#-- OUTPUT MODALS (not in display:none;) -->
    <@ectMarkupOut dir=(ectActionMsgModals!ectDefActionMsgModals) args={
        "actionProps": ectActionProps!{},
        "idPrefix": ectDialogIdPrefix,
        "idModalPrefix": ectDialogIdModalPrefix
    }/>
<#--</div>-->

<#-- TREE HANDLER CONFIG/INIT -->
<@script>
    var ectHandler = new ScpCatalogTreeHandler({
        treeId: "${escapeVal(ectTreeId!, 'js')}",
        productStoreEntity: <@objectAsScript object=(ectProductStore!productStore) lang='js'/>,
        actionProps: <@objectAsScript object=(ectActionProps!{}) lang='js'/>,
        hideShowFormIds: <@objectAsScript object=(ectAllHideShowFormIds![]) lang='js'/>,
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
        markupSelectors: <@objectAsScript object=ectDefMarkupSelectors lang='js'/>,
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
        dialogIdPrefix: "${escapeVal(ectDialogIdModalPrefix, 'js')}",
        objectLocFields: <@objectAsScript object=(ectObjectLocalizedFields!{}) lang='js' />
    });
    
    jQuery(document).ready(function() {
        ectHandler.initBindAll();
    });
</@script>

<#-- JSTREE CONFIG -->
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

<#-- MAIN CONTAINER -->
<@row>
  <@cell medium=9 large=9>

      <#-- JSTREE -->
      <@section title=uiLabelMap.ProductBrowseCatalogeAndCategories>
        <@treemenu id=ectTreeId settings=treeSettings plugins=treePlugins events=treeEvents>
            <#list treeMenuData as node>
                <#switch rawString(node.type)>
                    <#case "product">
                        <@treeitem text=(node.text!"") id=(node.id!) parent=(node.parent!"#") 
                            attribs={"data":{
                                "type": node.type!,
                                "li_attr": node.li_attr!{},
                                "productEntity": node.productEntity!{},
                                "productCategoryMemberEntity": node.productCategoryMemberEntity!{},
                                "isParent": node.isParent!false
                            }} 
                            state=(node.state!{})
                            icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}file"/>
                    <#break>
                    <#case "category">
                        <#assign text = rawString(node.text!"")>
                        <#assign sequenceNum = (node.productCategoryRollupEntity.sequenceNum)!(node.prodCatalogCategoryEntity.sequenceNum)!false>
                        <#if sequenceNum?is_number>
                          <#assign text = text + " #" + sequenceNum>
                        </#if>
                        <@treeitem text=text id=(node.id!) parent=(node.parent!"#") 
                            attribs={"data":{
                                "type": node.type!,
                                "li_attr": node.li_attr!{},
                                "productCategoryRollupEntity": node.productCategoryRollupEntity!{},
                                "prodCatalogCategoryEntity": node.prodCatalogCategoryEntity!{},
                                "productCategoryEntity": node.productCategoryEntity!{},
                                "isParent": node.isParent!false
                            }} 
                            state=(node.state!{})
                            <#-- FIXME?: the open-folder thing doesn't make sense... -->
                            icon=("${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}"+(node.state.opened?string("folder-open","folder")))/>
                    <#break>
                    <#case "catalog">
                        <#assign text = rawString(node.text!"")>
                        <#assign sequenceNum = (node.productStoreCatalogEntity.sequenceNum)!false>
                        <#if sequenceNum?is_number>
                          <#assign text = text + " #" + sequenceNum>
                        </#if>
                        <@treeitem text=text id=(node.id!) parent=(node.parent!"#") 
                            attribs={"data":{
                                "type": node.type!,
                                "li_attr": node.li_attr!{},
                                "productStoreCatalogEntity": node.productStoreCatalogEntity!{},
                                "prodCatalogEntity": node.prodCatalogEntity!{},
                                "isParent": node.isParent!false
                            }} 
                            state=(node.state!{})
                            icon="${styles.text_color_secondary!} ${styles.icon!} ${styles.icon_prefix!}cubes"/>
                    <#break>
                </#switch>
            </#list>
        </@treemenu>
      </@section>

      <#-- POST-JSTREE (ACTION FORMS, ETC.) -->
      <@ectMarkupOut dir=ectPostTreeArea!/>

  </@cell>
  <@cell medium=3 large=3>
    
      <#-- ACTIONS MENU -->
      <@section title=uiLabelMap.CommonActions>
        
        <#-- MENU -->
        <@ectMarkupOut dir=ectMarkupMenu!ectDefMarkupMenu/>
        
        <#-- DISPLAY EXTRAS (OPTIONS, ETC.) -->
        <@ectMarkupOut dir=ectActionExtrasArea!/>

      </@section>
      
      <#-- DISPLAY EXTRAS (OPTIONS, ETC.) -->
      <@ectMarkupOut dir=ectExtrasArea!/>
      
  </@cell>
</@row>
