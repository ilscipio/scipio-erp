<#include "pagescommon.ftl">

<#assign pageId = pvmsPageId!pageId>
<#assign webSiteId = pvmsWebSiteId!webSiteId>
<#assign editPageUrl = rawString(pvmsEditPageUrl!editPageUrl)>
<#assign pvmsTreeId = pvmsTreeId!"page-view-mappings-select">

<#assign pluginSettings={"three_state": false }/><#-- this seems too sketchy: "tie_selection": false -->
<#assign treePlugin =[{"name":"checkbox", "settings":pluginSettings}]/>

<@script>
jQuery(document).ready(function() {
    <#-- pre-selection script. we need this because setting "state" : { "checked" : true} in nodes isn't enough (unless tie_selection : false is added, but it's sketchy)
        and this way (select_node call) seems to trigger everything properly -->
    $("#${pvmsTreeId}").bind('ready.jstree', function(event, data) {
        var tree = $(this);
        var pageId = "${escapeVal(pageId, 'js')}";
        $(tree.jstree().get_json(tree, {
            flat: true
        })).each(function () {
            var node;
            if(this.data && this.data.active && this.data.pageId === pageId) {
                <#-- NOTE: here we can use either select_node or check_node. for some reason
                    select_node causes node expansion while check_node doesn't, so using
                    check_node for now; probably better anyway -->
                node = tree.jstree().check_node(this.id);
            }
        });
    });
});

<#-- can either use this or set string-list-suffix on service, not sure which better;
    for some reason no code is using string-list-suffix -->
function toServiceListArgStrRepr(arr) {
    var str = "[";
    for(var i=0; i < arr.length; i++) {
        if (i > 0) {
            str += ", "; <#-- NOTE: space is significant -->
        }
        str += arr[i];
    }
    return str + "]";
}

<#-- NOTE: this code works by detecting the changes the user made and submitting only those.
    this is optimal if done client-side; the only downside is forces need for tree refresh -->
function addRemovePageViewMappings(treeId) {
    var tree = $("#" + treeId);
    var pageId = "${escapeVal(pageId, 'js')}";
    
    <#-- find out which ID selections have changed since page load -->
    var addedIds = [];
    var removedIds = [];
    $(tree.jstree().get_json(tree, {
        flat: true
    })).each(function () {
        if(this.data && this.data.type == "view" && this.data.formalId) { <#-- only view nodes -->
            var checked = tree.jstree().is_checked(this.id);
            var wasActive = (this.data.active === true);
            
            if (wasActive && this.data.pageId === pageId && !checked) {
                removedIds.push(this.data.formalId);
            } else if (wasActive && this.data.pageId !== pageId && checked) {
                <#-- NOTE: this will automatically remove the mapping for the previous page -->
                addedIds.push(this.data.formalId); 
            } else if (!wasActive && checked) {
                addedIds.push(this.data.formalId);
            }
        }
    });
    
    if (addedIds.length > 0 || removedIds.length > 0) {
        <#--
        alert("added: " + addedIds.length);
        alert("removed: " + removedIds.length);
        -->

        if (true) {
            updateCmsElement("<@ofbizUrl escapeAs='js'>addRemovePageViewMappings</@ofbizUrl>", 
                {
                  "pageId" : "${escapeVal(pageId, 'js')}",
                  "addViewNameList" : toServiceListArgStrRepr(addedIds),
                  "removeViewNameList" : toServiceListArgStrRepr(removedIds)
                }, 
                function(eventMsgs) {
                    doCmsSuccessRedirect("${escapeFullUrl(editPageUrl, 'js')}", eventMsgs);
                }
            );
        }
    } else {
        <#-- TODO: nicer dialog -->
        alert("${escapeVal(rawLabel('CmsNoChangesMade'), 'js')}");
    }
}

</@script>

<style type="text/css">
    <#-- only way I could find to hide the parent checkboxes -->
    .page-view-mappings-select-container .jstree-anchor > .jstree-checkbox-disabled { display: none; }
</style>

<div class="page-view-mappings-select-container">
<@treemenu type="lib-basic" plugins=treePlugin id=pvmsTreeId> <#-- events=treeEvent -->
  <#list viewMaps.keySet() as key>
    <#if key?has_content && key != "noWebSiteId">
        <#assign currWebsite = viewMaps[rawString(key)]/>
        <#-- Add website -->
        <@treeitem text=key state=toSimpleMap(currWebsite.state!{}) id=(currWebsite.id!key) parent=(currWebsite.parent!"#") attribs={"data":{"type":"website","websiteid":currWebsite.webSiteId!""}} icon="${styles.text_color_secondary} ${styles.icon!} ${styles.icon_prefix!}folder"/>
        <#if currWebsite.views?has_content>
            <#list currWebsite.views?sort_by("text") as item>
                <#if item.data?has_content && item.data.type?has_content && item.data.type=="view">
                    <@treeitem attribs=item icon="${styles.text_color_primary} ${styles.icon!} ${styles.icon_prefix!}page ${styles.icon_prefix!}file-o"/>
                <#else>
                    <@treeitem attribs=item />
                </#if>
            </#list>
        </#if>
    </#if>
  </#list>
</@treemenu>
</div>

<br/>
<@menu type="button">
    <@menuitem type="link" href="javascript:addRemovePageViewMappings('${pvmsTreeId}');" class="+${styles.action_run_sys!} ${styles.action_update!}" text="${rawLabel('CmsSaveViewMappings')}" />
</@menu>  



