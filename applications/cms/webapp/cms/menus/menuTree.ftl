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
    
    var elData= {
                      text        : "string",
                      icon        : "fa fa-folder",
                      li_attr     : {},
                      a_attr      : {},
                      type        : 'test'
                    };
    function newMenuNode(){ 
         $('#cms-menu-tree').jstree().create_node('#', elData, 'last');
        return false;
    }
    
    function cmsAddPartialPathSuffix(path) {
        if (path) {
            if (path.endsWith("/")) {
                path += "...";
            } else {
                path += "/...";
            }
        } else {
            path = "/...";
        }
        return path;
    }
   
</@script>
<#-- 2017-10-11: there was a bug in this line PLUS jQuery no longer supports it:
  'dblclick.jstree':'openPageByNode(data.node);'
-->
<#assign treeEvent={'select_node.jstree':'updateMenu(data.node);'}/>
<#assign menuEventScript>
function($node) {
        var labelCreate = "${escapeVal(uiLabelMap.CommonCreate, 'js')}";
        var labelOpen = "${escapeVal(uiLabelMap.CommonOpen, 'js')}";
        var labelOverride = "${escapeVal(uiLabelMap.CmsOverride, 'js')}";
        var createDef = {
            "separator_before": false,
            "separator_after": false,
            "label": ($node.data.type=='request') ? labelOverride : labelCreate,
            "action": function (obj) { 
                tree.create_node($node.parent, JSON.stringify(elData), 'last');
            }
        };
        var openDef = {
            "separator_before": false,
            "separator_after": false,
            "label": labelOpen,
            "action":function (obj) {
                
            }
        };
        
        var renameDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonRename, 'js')}",
            "action": function (obj) { 
                tree.jstree('edit', $node);
            }
        };                         
        var removeDef = {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            "action": function (obj) { 
                tree.jstree('delete_node', $node);
            }
        };
        
         return {
            "Open": openDef,
            "Create": createDef,
            "Rename": renameDef,
            "Remove": removeDef
        }
    }
</#assign>
<#assign pluginSettings={"items": wrapRawScript(menuEventScript)}/>
<#assign treePlugin =[{"name":"contextmenu", "settings":pluginSettings},{"name":"massload"}]/>

<#-- Content -->
<@section>
    <@row>
           <#-- JSTree displaying all content nodes -->
            <@cell columns=9>
                <@section title=uiLabelMap.CmsMenu>
                   <@treemenu type="lib-basic" events=treeEvent plugins=treePlugin id="cms-menu-tree"> 
                    </@treemenu>
                </@section>
            </@cell>
            
            <#-- empty side menu -->
            <@cell columns=3>
                <@section title=uiLabelMap.CmsMenu id="action_offset">
                        <ul class="side-nav" id="action_menu">
                            <@menuitem type="link" href="javascript:newMenuNode();" text=uiLabelMap.CommonCreate/>
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