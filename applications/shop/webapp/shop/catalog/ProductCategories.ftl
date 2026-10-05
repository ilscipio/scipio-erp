<#--
Scipio Commerce
Copyright (C) Ilscipio GmbH

This file is part of Scipio Commerce. Scipio Commerce is free software: you
can redistribute it and modify it under the terms of the GNU Affero General
Public License, version 3, as published by the Free Software Foundation.
Scipio Commerce is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
for more details. You should have received a copy of the license with this
work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
A commercial license is available from Ilscipio GmbH.

SPDX-License-Identifier: AGPL-3.0-only
-->
<#include "component://shop/webapp/shop/catalog/catalogcommon.ftl">

<#--
<@script src=makeContentUrl("/images/jquery/plugins/jsTree/jquery.jstree.js") />
<@script src=makeContentUrl("/images/jquery/ui/js/jquery.cookie-1.4.0.js") />-->
  
<@script>
<#-- some labels are not unescaped in the JSON object so we have to do this manuely -->
function unescapeHtmlText(text) {
    return jQuery('<div />').html(text).text()
}

jQuery(window).load(createTree());

<#-- creating the JSON Data -->
var rawdata = [
  <#if (requestAttributes.topLevelList)??>
    <#assign topLevelList = requestAttributes.topLevelList>
  </#if>
  <#if (topLevelList?has_content)>
    <@fillTree rootCat=completedTree/>
  </#if>
  
  <#macro fillTree rootCat>
  <#if (rootCat?has_content)>
    <#list rootCat?sort_by("productCategoryId") as root>
            {
            "data": {"title" : unescapeHtmlText("<#if root.categoryName??>${root.categoryName?js_string}<#elseif root.categoryDescription??>${root.categoryDescription?js_string}<#else>${root.productCategoryId?js_string}</#if>"), "attr": { "href":"javascript: void(0);", "onClick":"callDocument('${root.productCategoryId?js_string}', '${root.parentCategoryId?js_string}')" , "class" : "${root.cssClass!}"}},
            "attr": {"id" : "${root.productCategoryId?js_string}"}
            <#if root.child?has_content>
                ,"children": [
                    <@fillTree rootCat=root.child/>
                    ]
            </#if>
            <#if root_has_next>
                },
            <#else>
                }
            </#if>
    </#list>
  </#if>
</#macro>
     ];

 <#-------------------------------------------------------------------------------------define Requests-->
  var editDocumentTreeUrl = '<@pageUrl>views/EditDocumentTree</@pageUrl>';
  var listDocument =  '<@pageUrl>views/ListDocument</@pageUrl>';
  var editDocumentUrl = '<@pageUrl>views/EditDocument</@pageUrl>';
  var deleteDocumentUrl = '<@pageUrl>removeDocumentFromTree</@pageUrl>';

 <#-------------------------------------------------------------------------------------create Tree-->
  function createTree() {
    jQuery(function () {
        jQuery("#tree").jstree({
        "themes" : {
            "theme" : "classic",
            "icons" : false
        },
        "cookies" : {
            "cookie_options" : {path: '/'} 
        },
       "plugins" : [ "themes", "json_data", "cookies"],
            "json_data" : {
                "data" : rawdata
            }
        });
    });
  }

<#-------------------------------------------------------------------------------------callDocument function-->
    function callDocument(id, parentCategoryStr) {
        var checkUrl = '<@pageUrl>productCategoryList</@pageUrl>';
        if(checkUrl.search("http"))
            var ajaxUrl = '<@pageUrl>productCategoryList</@pageUrl>';
        else
            var ajaxUrl = '<@pageUrl>productCategoryListSecure</@pageUrl>';

        //jQuerry Ajax Request
        jQuery.ajax({
            url: ajaxUrl,
            type: 'POST',
            data: {"category_id" : id, "parentCategoryStr" : parentCategoryStr},
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#div3').html(msg);
            }
        });
     }
<#-------------------------------------------------------------------------------------callCreateDocumentTree function-->
      function callCreateDocumentTree(contentId) {
        jQuery.ajax({
            url: editDocumentTreeUrl,
            type: 'POST',
            data: {contentId: contentId,
                        contentAssocTypeId: 'TREE_CHILD'},
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#Document').html(msg);
            }
        });
    }
<#-------------------------------------------------------------------------------------callCreateSection function-->
    function callCreateDocument(contentId) {
        jQuery.ajax({
            url: editDocumentUrl,
            type: 'POST',
            data: {contentId: contentId},
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#Document').html(msg);
            }
        });
    }
<#-------------------------------------------------------------------------------------callEditSection function-->
    function callEditDocument(contentIdTo) {
        jQuery.ajax({
            url: editDocumentUrl,
            type: 'POST',
            data: {contentIdTo: contentIdTo},
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#Document').html(msg);
            }
        });

    }
<#-------------------------------------------------------------------------------------callDeleteItem function-->
    function callDeleteDocument(contentId, contentIdTo, contentAssocTypeId, fromDate) {
        jQuery.ajax({
            url: deleteDocumentUrl,
            type: 'POST',
            data: {contentId : contentId, contentIdTo : contentIdTo, contentAssocTypeId : contentAssocTypeId, fromDate : fromDate},
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                location.reload();
            }
        });
    }
 <#-------------------------------------------------------------------------------------callRename function-->
    function callRenameDocumentTree(contentId) {
        jQuery.ajax({
            url: editDocumentTreeUrl,
            type: 'POST',
            data: {  contentId: contentId,
                     contentAssocTypeId:'TREE_CHILD',
                     rename: 'Y'
                     },
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#Document').html(msg);
            }
        });
    }
 <#------------------------------------------------------pagination function -->
    function nextPrevDocumentList(url){
        url= '<@pageUrl>'+url+'</@pageUrl>';
         jQuery.ajax({
            url: url,
            type: 'POST',
            error: function(msg) {
                alert("An error occurred loading content! : " + msg);
            },
            success: function(msg) {
                jQuery('#Document').html(msg);
            }
        });
    }

</@script>

<@section title=uiLabelMap.ProductCategories id="quickadd">
    <div id="tree">
    </div>
</@section>
