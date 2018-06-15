<#include "../common/common.ftl">

<#-- 
DEV NOTE: MOST OF OUR CODE CURRENTLY ASSUMES primaryPathFromContextRoot(Default)=Y
    This means by default we display /control prefix everywhere so ALL paths
    are relative to webapp context root, consistently.
    This is the simplest/best default because it allows the greatest flexibility with
    the simplest syntax (only a single path needed to express, no matter if page or request,
    always from webapp context root).
-->

<#-- Javascript functions -->
<@script>
    var editorBaseUrl = '<@ofbizUrl escapeAs='js'>menus</@ofbizUrl>';
    var menuId='';
    var websiteId = 'cmsSite';
    var elData= {
                  text        : "New Link",
                  icon        : "${styles.text_color_primary} ${styles.icon!} ${styles.icon_prefix!}page ${styles.icon_prefix!}file-o", <#-- Note: Ideally this should not be stored here. But it simplifies the process for now.-->
                  li_attr     : {},
                  a_attr      : {},
                  data : {
                    type        : "link_internal"
                  }
                };
    
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
            });
        }
    };
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
        menuTree.delete_node(sel,null, function(){
            saveMenu();
        });
    };
    
    function refreshMenuData(newData){
        var menuTree = $('#cms-menu-tree').jstree(true);
        menuTree.settings.core.data = newData;
        menuTree.refresh();
        return false;
    }
    
    <#-- Function to load menu from db -->
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
        var v =  $('#cms-menu-tree').jstree(true).get_json('#', {flat:true})
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
              "${escapeVal(uiLabelMap.CommonRename, 'js')}": makeLink($node,'edit'),
              "${escapeVal(uiLabelMap.CommonRemove, 'js')}": makeLink($node,'remove')
            };
        } else {
            newOptions = {
              "${escapeVal(uiLabelMap.CommonCreate, 'js')}": makeLink($node,'create'),
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
    
    <#-- Load after the site has been initialized -->
    $(function() {
        loadMenu();
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
        var removeDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            "action": function (obj) { 
               node_delete();
            }
        };
        

        return {
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
        <@field type="select" name="activeMenu" id="activeMenu" onChange="loadMenu();">
            <#list cmsMenus as cmsMenu>
               <option value="${cmsMenu.menuId!""}">${cmsMenu.menuName!cmsMenu.menuId!""}</option>
            </#list>
        </@field>
        <@menuitem type="link" href="javascript:addMenu(); void(0);" class="+${styles.action_run_sys!} ${styles.action_create!}" text=uiLabelMap.CmsWebSiteAddMenu/>
        <@menuitem type="link" href="javascript:deleteMenu(); void(0);" class="+${styles.action_run_sys!} ${styles.action_remove!} action_delete" text=uiLabelMap.CommonDelete/>
    </@menu>  
</#macro>

<@section menuContent=menuContent>
    <@row>
           <#-- JSTree displaying all content nodes -->
            <@cell columns=9>
                <@section title=uiLabelMap.CmsMenus>
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