<#include "../common/common.ftl">

<#-- 
DEV NOTE: MOST OF OUR CODE CURRENTLY ASSUMES primaryPathFromContextRoot(Default)=Y
    This means by default we display /control prefix everywhere so ALL paths
    are relative to webapp context root, consistently.
    This is the simplest/best default because it allows the greatest flexibility with
    the simplest syntax (only a single path needed to express, no matter if page or request,
    always from webapp context root).
-->

<#-- Pre-configured Modals -->
<@modal id="delete-dialog">
    <@heading>${uiLabelMap.CommonWarning}</@heading>
    ${uiLabelMap.CmsConfirmDeleteAction}
    <div class="modal-footer ${styles.text_right}">
       <a id="delete-button" class="${styles.button} btn-ok">${uiLabelMap.CommonContinue}</a>
    </div>
</@modal>
<@modal id="edit-menuitem-dialog">
    <@heading>${uiLabelMap.CommonEdit}</@heading>
    <@form id="edit_menu_item">
        <@field type="select" name="type" id="type" label=uiLabelMap.CmsLinkType>
               <option value="link_internal">${uiLabelMap.CmsLinkInternal}</option>
               <option value="link_external">${uiLabelMap.CmsLinkExternal}</option>
        </@field>
        <@field type="input" id="path" name="path" value="" label=uiLabelMap.CmsMenuItemLink placeholder="/main" required=true/>
        <div class="modal-footer ${styles.text_right}">
            <@field type="submit" text=uiLabelMap.CommonEdit class="${styles.link_run_sys!} ${styles.action_edit!}" />
        </div>
    </@form>
</@modal>

<#-- Javascript functions -->
<@script>
    var editorBaseUrl = '<@ofbizUrl escapeAs='js'>menus</@ofbizUrl>';
    var menuId='';
    var websiteId = 'cmsSite';
    var nodeIcons = {
        link_internal : "${styles.text_color_primary} ${styles.icon!} ${styles.icon_prefix!}page ${styles.icon_prefix!}file-o",
        pages : "${styles.text_color_primary} ${styles.icon!} ${styles.icon_prefix!}page ${styles.icon_prefix!}file-o"
    };
    var elData= {
                  text        : "New Link",
                  icon        : nodeIcons["link_internal"], <#-- Note: Ideally this should not be stored here. But it simplifies the process for now.-->
                  li_attr     : {},
                  a_attr      : {},
                  data : {
                    type        : "link_internal"
                  }
                };
    
    
    function open_dialog(dialogName){
        try {
                $('#'+dialogName).foundation('reveal','open');
            } catch(err) {
                try {
                    $('#'+dialogName).modal('show'); 
                }
                catch(err) {
                    t.dispatchEvent(event);
                }
            }
        return false;
    }
    
    function close_dialog(dialogName){
        try {
                $('#'+dialogName).foundation('reveal','close');
            } catch(err) {
                try {
                    $('#'+dialogName).modal('hide'); 
                }
                catch(err) {
                    t.dispatchEvent(event);
                }
            }
        return false;
    }
    
    <#-- Functions to manage menu items -->
    function node_create(data,isParent) {
        var menuTree = $('#cms-menu-tree').jstree(true);
        var sel;
        if(isParent){
            sel = menuTree.create_node('#', data);
        }else{
            sel = menuTree.get_selected();
            if(!sel.length) { return false; }
            sel = sel[0];
            sel = menuTree.create_node(sel, data);
        }
        
        if(sel) {
            menuTree.edit(sel,null, function(){
               saveMenu();
               menuTree.deselect_all();
               menuTree.select_node(sel);
               node_edit();
            });
        }
    };
    
    function node_edit() {
        var menuTree = $('#cms-menu-tree').jstree(true);
        var sel = menuTree.get_selected();
        if(!sel.length) { return false; }
        sel = sel[0];
        var node = menuTree.get_node(sel);
        $('form#edit_menu_item')[0].reset();
        if(node && node["data"]){
            if(node["data"]["type"]){
                $('form#edit_menu_item #type').val(node["data"]["type"]).prop('selected', true);
            }
            if(node["data"]["path"]){
                $('form#edit_menu_item #path').val(node["data"]["path"]);
            }
        }
        open_dialog('modal_edit-menuitem-dialog');
    };
    
    function node_update(){
        var menuTree = $('#cms-menu-tree').jstree(true);
        var path = $('form#edit_menu_item #path').val();
        var type = $('form#edit_menu_item #type').val();
        var sel = menuTree.get_selected();
            if(!sel.length) { return false; }
            sel = sel[0];
        var node = menuTree.get_node(sel);
        if(node) {
            node["data"]["type"] = type;
            node["data"]["path"] = path;
            node["icon"] = nodeIcons[type];
            close_dialog('modal_edit-menuitem-dialog');
            saveMenu();
            menuTree.redraw_node(sel);
        }
        return false;
    }
    
    function node_rename() {
        var menuTree = $('#cms-menu-tree').jstree(true);
        var sel = menuTree.get_selected();
        if(!sel.length) { return false; }
        sel = sel[0];
        menuTree.edit(sel,null, function(){
            saveMenu();
        });
    };
    function node_delete() {
        var menuTree = $('#cms-menu-tree').jstree(true);
        var sel = menuTree.get_selected();
        if(!sel.length) { return false; }
        menuTree.delete_node(sel);
        saveMenu();
    };
    
    function refreshMenuData(newData){
        var menuTree = $('#cms-menu-tree').jstree(true);
        menuTree.settings.core.data = newData;
        menuTree.refresh();
        return false;
    }
    
    <#-- Function to manage menus -->
    function addMenu(){
        var menuName = $('#menuname').val();
        var data = {menuName:menuName,menuJson:"[]"};
        $.ajax({
                  type: "POST",
                  url: "<@ofbizUrl escapeAs='js'>saveMenu</@ofbizUrl>",
                  data: data,
                  cache:false,
                  async:true,
                  success: function(data) { 
                        menuId = data.menuId;
                        $('#activeMenu').append($('<option>', { 
                            value: menuId,
                            text : menuName
                        }));
                        $("#activeMenu").val(menuId).prop('selected', true);
                        refreshMenuData(JSON.parse(data.menuJson));
                      }
            });
            close_dialog('modal_create-menu-dialog');
        return false;
    }
    function loadMenu(){
        menuId = $('#activeMenu').val();
        var data = {menuId:menuId};
        $.ajax({
                  type: "POST",
                  url: "<@ofbizUrl escapeAs='js'>getMenu</@ofbizUrl>",
                  data: data,
                  cache:false,
                  async:true,
                  success: function(data) { 
                        refreshMenuData(JSON.parse(data.menuJson));
                      }
            });
        return false;
    
    }
    
    function saveMenu() {
        var v =  $('#cms-menu-tree').jstree(true).get_json('',{flat:true});
        var menuJson = JSON.stringify(v);
        var data = {websiteId:websiteId,menuJson:menuJson};
        if(menuId){
            data.menuId=menuId;
        }
        $.ajax({
                  type: "POST",
                  url: "<@ofbizUrl escapeAs='js'>saveMenu</@ofbizUrl>",
                  data: data,
                  cache:false,
                  async:true,
                  success: function(data) { 
                        menuId = data.menuId;
                      }
            });
        return false;
    };
    
    function deleteMenu() {
            menuId = $('#activeMenu').val();
            var data = {menuId:menuId};
            $.ajax({
                  type: "POST",
                  url: "<@ofbizUrl escapeAs='js'>deleteMenu</@ofbizUrl>",
                  data: data,
                  cache:false,
                  async:true,
                  success: function(data) { 
                        $("#activeMenu option[value='"+menuId+"']").remove();
                        loadMenu();
                      }
            });
            close_dialog('modal_delete-dialog');
        }
    
    function makeLink($node,action) {
        var retStr="javascript:";
        switch(action) {
            case "create":
                retStr=retStr.concat("node_create(elData,false);");
                break;
            case "open":
                break;
            case "edit":
                 retStr=retStr.concat("node_rename();");
                break;
            case "remove":
                 retStr=retStr.concat("node_delete();");
                break; 
            case "update":
                 retStr=retStr.concat("node_edit();");
                break;    
            case "createroot":
                retStr=retStr.concat("node_create(elData,true);");
                break;       
            default:
        }
        retStr.concat("return false;");
        return retStr;
    }
    
    <#-- Function to update the action menu. Will generate new menu items based on selected option -->
    function updateMenu($node){
        var $el = $("#action_menu");

        var newOptions;
        if ($node.data && $node.data.type=='root') {
            newOptions = {
              "${escapeVal(uiLabelMap.CommonCreate, 'js')}": makeLink($node,'create'),
              "${escapeVal(uiLabelMap.CommonEdit, 'js')}": makeLink($node,'update'),
              "${escapeVal(uiLabelMap.CommonRename, 'js')}": makeLink($node,'edit'),
              "${escapeVal(uiLabelMap.CommonRemove, 'js')}": makeLink($node,'remove')
            };
        } else {
            newOptions = {
              "${escapeVal(uiLabelMap.CommonCreate, 'js')}": makeLink($node,'create'),
              "${escapeVal(uiLabelMap.CommonEdit, 'js')}": makeLink($node,'update'),
              "${escapeVal(uiLabelMap.CommonRename, 'js')}": makeLink($node,'edit'),
              "${escapeVal(uiLabelMap.CommonRemove, 'js')}": makeLink($node,'remove')
            }; 
        }
            
        $el.empty(); // remove old options
        $.each(newOptions, function(key,value) {
          var newEl = $('<@compress_single_line><@menuitem type="link" href="" text=""/></@compress_single_line>');
          var menuAnchor = $(newEl).find('a:last-child');
          menuAnchor.attr("href",value).text(key);
          $el.append(newEl);
        });
    }
    <@commonCmsScripts />
    
    <#-- Load after the site has been initialized -->
    $(function() {
        loadMenu();
        setupCmsDeleteActionHooks();
        
        $("#create_menu_form").submit(function(e){
            addMenu();
            return false;
        });
        
        $("#edit_menu_item").submit(function(e){
            node_update();
            return false;
        });

    });    
   
</@script>
<#assign treeEvent={'select_node.jstree':'updateMenu(data.node);'}/>
<#assign menuEventScript>
function($node) {
        var labelCreate = "${escapeVal(uiLabelMap.CommonCreate, 'js')}";
        var labelOpen = "${escapeVal(uiLabelMap.CommonOpen, 'js')}";
        var labelOverride = "${escapeVal(uiLabelMap.CmsOverride, 'js')}";
        var createDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonCreate, 'js')}",
            "action": function (obj) { 
                node_create(elData,false);
            }
        };
        var openDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonOpen, 'js')}",
            "action":function (obj) {
                
            }
        };
        
        var renameDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonRename, 'js')}",
            "action": function (obj) { 
               node_rename();
            }
        };  
        
       var updateDef = {
            "separator_before": true,
            "separator_after": true,
            "label": "${escapeVal(uiLabelMap.CommonEdit, 'js')}",
            "action": function (obj) { 
               node_edit();
            }
        };  
                               
        var removeDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            "action": function (obj) { 
               node_delete();
            }
        };
        

        return {
            "Update": updateDef,
            "Open": openDef,
            "Create": createDef,
            "Rename": renameDef,
            "Remove": removeDef
        };

    }
</#assign>
<#assign pluginSettings={"items": wrapRawScript(menuEventScript)}/>
<#assign treePlugin =[{"name":"contextmenu", "settings":pluginSettings},{"name":"massload"},{"name":"dnd"}]/>

<#-- Content -->
<#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@field type="select" name="activeMenu" id="activeMenu" onChange="loadMenu();" label=uiLabelMap.CmsCurrentMenu>
            <#list cmsMenus as cmsMenu>
               <option value="${cmsMenu.menuId!""}">${cmsMenu.menuName!cmsMenu.menuId!""}</option>
            </#list>
        </@field>
        <@menuitem type="generic">
            <@modal id="create-menu-dialog" label=uiLabelMap.CmsWebSiteAddMenu linkClass="+${styles.menu_button_item_link!} ${styles.action_nav!} ${styles.action_create!}">
                <@heading>${uiLabelMap.CmsWebSiteAddMenu}</@heading>
                    <@form id="create_menu_form">
                        <@field type="input" id="menuname" name="menuName" value="" label=uiLabelMap.CmsMenuName placeholder="menu name" required=true/>
                        <div class="modal-footer ${styles.text_right}">
                            <@field type="submit" text=uiLabelMap.CommonAdd class="${styles.link_run_sys!} ${styles.action_add!}" />
                        </div>
                    </@form>
            </@modal>
        </@menuitem>
        <@menuitem type="link" href="javascript:deleteMenu(); void(0);" class="+${styles.action_run_sys!} ${styles.action_remove!} action_delete" text=uiLabelMap.CommonDelete/>
    </@menu>  
</#macro>

<@section menuContent=menuContent title=uiLabelMap.CmsMenus>
    <@row>
           <#-- JSTree displaying all content nodes -->
            <@cell columns=9>
                <@section title=uiLabelMap.CmsEditMenu>
                   <@treemenu type="lib-basic" events=treeEvent plugins=treePlugin id="cms-menu-tree">
                   </@treemenu>
                </@section>
            </@cell>
            
            <#-- empty side menu -->
            <@cell columns=3>
                <@section title=uiLabelMap.CmsMenu id="action_offset">
                        <ul class="side-nav" id="action_menu">
                            <@menuitem type="link" href="javascript:node_create(elData,true);" text=uiLabelMap.CommonCreate/>
                        </ul>
                </@section>
                <#--  Page list to enable drag & drop in between menus -->
                <@section title=uiLabelMap.CmsPages>
                   <#assign pageTreePlugin =[{"name":"massload"},{"name":"dnd", "settings": {"always_copy":true}}]/>
                   <#assign pageTreeCallback>
                    function (operation, node, node_parent, node_position, more) {
                        if('copy_node'==operation){
                            node.data.type="link_internal";
                            saveMenu();
                        }
                        
                        return true;
                    }
                   </#assign>
                   <#assign pageTreeSettings = {
                            "multiple": false, <#-- TODO: in future could implement partial multiple operations (remove/move/copy) -->
                            "check_callback": wrapRawScript(pageTreeCallback)}/>
                   <@treemenu type="lib-basic" id="cms-content-tree" plugins=pageTreePlugin settings=pageTreeSettings> 
                        <#list requestMaps.keySet() as key>
                            <#if key?has_content && key!="noWebSiteId">
                                <#assign currWebsite= requestMaps[key]/>
                                <#assign treeWebsiteDisabled = false/>
                                <#if currWebsite.enabled?has_content && currWebsite.enabled=="false">
                                    <#assign treeWebsiteDisabled = true/>
                                </#if>
                                <#-- Add website -->
                                <@treeitem text=key state={"disabled": treeWebsiteDisabled} id=(currWebsite.id!key) parent=(currWebsite.parent!"#") 
                                    attribs={"data":{"type":"website","websiteid":currWebsite.webSiteId!"", 
                                        "editorRequestPathPrefix":currWebsite.editorRequestPathPrefix!"", "primaryPathFromContextRootDefault":currWebsite.primaryPathFromContextRootDefault!""}} 
                                    icon="${styles.text_color_secondary} ${styles.icon!} ${styles.icon_prefix!}folder"/>
                                <#if currWebsite.pages?has_content>
                                    <#-- Add requests and pages -->
                                    <#list currWebsite.pages?sort_by("text") as item>
                                        <#if ((item.data.type)!"")=="page">                                            
                                            <@treeitem attribs=item a_attr=itemPath icon="${styles.text_color_primary} ${styles.icon!} ${styles.icon_prefix!}page ${styles.icon_prefix!}file-o"/>
                                        <#elseif ((item.data.type)!"")=="request">
                                            <@treeitem attribs=item a_attr=itemPath   icon="${styles.text_color_info} ${styles.icon!} ${styles.icon_prefix!}link"/>
                                        <#else>
                                            <@treeitem attribs=item a_attr=itemPath/>
                                        </#if>
                                    </#list>
                                </#if>
                            </#if>
                       </#list>
                    </@treemenu>
                    <@script>
                        $(function() {
                            <#-- 
                            $('#cms-content-tree, #cms-menu-tree').on("copy_node.jstree", function (e, data) { 
                              data.original.data.type="link_internal";
                            });-->
                        });    
                    </@script>
                </@section>
                
                <#-- special message for root-aliasing of controller URIs - loaded by JS -->
                <div id="cms-ctrlrootalias-msgarea" class="cms-ctrlrootalias-msgarea" style="display:none;">
                    <@alert type="info"><span class="cms-ctrlrootalias-msg"></span></@alert>
                </div>
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