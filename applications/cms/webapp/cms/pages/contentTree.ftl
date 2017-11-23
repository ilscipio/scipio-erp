<#include "pagescommon.ftl">

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
    var editorBaseUrl = '<@ofbizUrl escapeAs='js'>editPage</@ofbizUrl>';
    
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
    
    function makeCmsNewPageUrl($node) {
        var newPath;
        var path = $node.data["path"]; //$node.a_attr.href
        if ($node.data.type=='request' && $node.data.isTargetPath === true) {
            newPath = getCreatePathPrefix($node) + path;
        } else {
            newPath = cmsAddPartialPathSuffix(path);
        }
        var newUrl = editorBaseUrl + '?' + $.param({
            webSiteId: $node.data["websiteid"],
            path: newPath,
            isNewPage: "Y"
        });
        return newUrl;
    }
    
    function makeCmsEditPageUrl($node) {
        var path = $node.data["path"]; //$node.a_attr.href
        var editorUrl = editorBaseUrl + '?' + $.param({
            webSiteId: $node.data["websiteid"],
            path: path
        });
        return editorUrl;
    }
    
    <#-- Function to update the action menu. Will generate new menu items based on selected option -->
    function updateMenu($node){
        var $el = $("#action_menu");

        var newOptions;
        if ($node.data.type=='website') {
            newOptions = {
              "${escapeVal(uiLabelMap.CommonCreate, 'js')}": makeCmsNewPageUrl($node)
            }; 
        } else if($node.data.type=='request') {
            newOptions = {
              "${escapeVal(uiLabelMap.CmsOverride, 'js')}": makeCmsNewPageUrl($node)
            };
        } else if($node.data.type=='page') {
            newOptions = {
              "${escapeVal(uiLabelMap.CommonCreate, 'js')}": makeCmsNewPageUrl($node),
              "${escapeVal(uiLabelMap.CommonOpen, 'js')}": makeCmsEditPageUrl($node)
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
    
    function openPageByNode($node){
        document.location.href = makeCmsEditPageUrl($node);
        return false;
    }
    
    <#-- Returns path prefix for the node, but only if not configured to be accounted for
        in the node paths (only if primaryPathFromContextRootDefault is not "Y") -->
    function getCreatePathPrefix($node) {
        <#-- get parent node (the website node) FIXME: couldn't find direct function on $node -->
        var $parentNode = jQuery('#cms-content-tree').jstree().get_node($node.parent);
        
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
                document.location.href = makeCmsNewPageUrl($node);
            }
        };
        var openDef = {
            "separator_before": false,
            "separator_after": false,
            "label": labelOpen,
            "action":function (obj) {
                document.location.href = makeCmsEditPageUrl($node);
            }
        };

        if ($node.data.type=='website') {
            return {
                "Create": createDef
            };
        } else if ($node.data.type=='request') {
            return {
                "Create": createDef
            };
        } else if ($node.data.type=='page') {
            return {
                "Open": openDef,
                "Create": createDef
            };
        } else {
            return {};
        }
        <#--,
        "Rename": {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonRename, 'js')}",
            "action": function (obj) { 
                tree.jstree('edit', $node);
            }
        },                         
        "Remove": {
            "separator_before": false,
            "separator_after": false,
            "label": "${escapeVal(uiLabelMap.CommonRemove, 'js')}",
            "action": function (obj) { 
                tree.jstree('delete_node', $node);
            }
        }
            };
        -->
    }
</#assign>
<#assign pluginSettings={"items": wrapRawScript(menuEventScript)}/>
<#assign treePlugin =[{"name":"contextmenu", "settings":pluginSettings},{"name":"massload"}]/>

<#-- Content -->
<@section>
    <@row>
            <#-- JSTree displaying all content nodes -->
            <@cell columns=9>
                <@section title=uiLabelMap.CmsContent>
                   <@treemenu type="lib-basic" events=treeEvent plugins=treePlugin id="cms-content-tree"> 
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
                                            <@treeitem attribs=item a_attr=itemPath icon="${styles.text_color_info} ${styles.icon!} ${styles.icon_prefix!}link"/>
                                        <#else>
                                            <@treeitem attribs=item a_attr=itemPath/>
                                        </#if>
                                    </#list>
                                </#if>
                            </#if>
                       </#list>
                    </@treemenu>
                </@section>
            </@cell>
            
            <#-- empty side menu -->
            <@cell columns=3>
                <@section title=uiLabelMap.CmsMenu id="action_offset">
                        <ul class="side-nav" id="action_menu">
                            <@menuitem type="link" href=makeOfbizUrl('editPage') text=uiLabelMap.CommonCreate/>
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