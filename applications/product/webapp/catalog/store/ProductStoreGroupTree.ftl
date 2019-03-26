<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@script>
<#-- some labels are not unescaped in the JSON object so we have to do this manualy -->
function unescapeHtmlText(text) {
    return jQuery('<div />').html(text).text()
}

jQuery(window).load(createTree());

<#-- creating the JSON Data -->
<#if parentProductStoreGroup?has_content>
    <#assign parentGroupList = [parentProductStoreGroup]>
<#else>
    <#assign parentGroupList = parentProductStoreGroups>
</#if>
var rawdata = [
    <#list parentGroupList as parentGroup>
                   {
                    "data": {"title" : unescapeHtmlText("<#if parentGroup.productStoreGroupName??>${parentGroup.productStoreGroupName?js_string} [${parentGroup.productStoreGroupId?js_string}]</#if>"),
                                  "attr": {"href" : "<@pageUrl>EditProductStoreGroupAndAssoc</@pageUrl>","onClick" : "callDocument('${parentGroup.productStoreGroupId?js_string}');"}},
                    "attr": {"parentGroupId" : "${parentGroup.productStoreGroupId?js_string}"}, 
                    "state" : "closed"
                    }<#if parentGroup_has_next>,</#if>
     </#list>
     ];

 <#-- create Tree-->
  function createTree() {
    jQuery(function () {
        jQuery("#tree").jstree({
        "plugins" : [ "themes", "json_data","ui" ,"cookies", "types"],
            "json_data" : {
                "data" : rawdata,
                "ajax" : { "url" : "<@pageUrl>getProductStoreGroupRollupHierarchy</@pageUrl>",
                           "type" : "POST",
                           "data" : function (n) {
                               return {
                                   "parentGroupId" :  n.attr ? n.attr("parentGroupId").replace("node_","") : 1,
                                   "onclickFunction" : "callDocument"
                               };
                           },
                           success : function (data) {
                               return data.storeGroupTree;
                           }
                }
            },
            "types" : {
                "valid_children" : [ "root" ]
            }
        });
    });
  }
  
  function callDocument(id) {
    //jQuerry Ajax Request
    var dataSet = {};
    dataSet = {"productStoreGroupId" : id, "ajaxUpdateEvent" : "Y"};
    jQuery.ajax({
        url: 'EditProductStoreGroupAndAssoc',
        type: 'POST',
        data: dataSet,
        error: function(msg) {
            alert("An error occurred loading content! : " + msg);
        },
        success: function(msg) {
            jQuery('#centerdiv').html(msg);
        }
    });
  }
</@script>

<div id="tree"></div>
