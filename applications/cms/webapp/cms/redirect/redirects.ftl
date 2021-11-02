<#include "component://cms/webapp/cms/common/common.ftl">
<@script>
    var currentTable;
    function openSection(v){
        var divsToHide = document.getElementsByClassName("wb_entry");
        for(var i = 0; i < divsToHide.length; i++){
            divsToHide[i].style.display = "none";
        }
        currentTable  = $('#table_'+v).DataTable( {
             "destroy": true,
             "fixedHeader" : true,
             "info" : true,
             "paging" : false,
             "searching": true,
             "table-layout": "fixed",
             "rowId": "from",
             "ajax": {
                "destroy": true,
                "url": '<@pageUrl>getRedirects</@pageUrl>',
                "type": "POST",
                "data": {websiteId:v},
                "dataSrc": "redirectsJson"
             },
             "columns": [
                    { data: "from" },
                    { data: "to" },
                    { data: "type" , "searchable": false}
             ],
             "columnDefs": [ {
                "targets": 3,
                "data": "edit_link",
                "render": function ( data, type, row, meta ) {
                    return "<a href='#' onClick='editRow(" + meta.row + ");'>${uiLabelMap.CommonEdit}</a>";
                }
             },
             {
                "targets": 4,
                "data": "delete_link",
                "render": function ( data, type, row, meta ) {
                    return "<a href='#' onClick='deleteRow(" + meta.row + ");' class='${styles.text_color_error}'>${uiLabelMap.CommonDelete}</a>";
                }
              } ]
          });

        document.getElementById('edit_'+v).style.display = "block";

        //JSON.stringify(currentTable.data().toArray());
    }


    function editRow(index){
        var row = currentTable.row(index);
        var rowData = row.data();
        var rowNode = row.node();
        $('#row_index').val(index);
        $('#row_from').val(rowData.from);
        $('#row_to').val(rowData.to);
        $('#row_type').val(rowData.type);
        return false;
    }

    function deleteRow(index){
        var row = currentTable.row(index);
        var rowNode = row.node();
        row.remove().draw();
        saveAllRedirects();
        return false;
    }

    function saveAllRedirects(){
        var params = {
            webSiteId: $('#scriptWebsiteId').val(),
            redirects: JSON.stringify(currentTable.data().toArray())
        };
        $.ajax({
            type: "POST",
            url: '<@pageUrl>updateRedirects</@pageUrl>',
            data: params,
            cache: false,
            async: true,
            success: function(data) {

            }
        });

        return true;
    }

    function saveRedirect(e){
        this.event.preventDefault();
        var from = $('#row_from').val();
        var to = $('#row_to').val();
        var type = $('#row_type').val();
        var index = $('#row_index').val();
        var rowNode;
        if(index){
            var row = currentTable.row(index);
            var newData = {"from": from,"to": to,"type":type};
            row.data(newData).invalidate().draw();
            rowNode = row.node();
        }else{
            rowNode = currentTable.row.add({"from": from,"to": to,"type":type}).draw().node();
        }
        saveAllRedirects();
        $('#row_index').val('');
        $('#row_from').val('');
        $('#row_to').val('');
        $('#row_type').val('301');
        return true;
    }

    window.addEventListener("load", function(){
        openSection(document.getElementById('scriptWebsiteId').value);
    });
</@script>
<#if websites?has_content>
    <@section>
        <@row>
            <@cell columns=12>
                <@section title=uiLabelMap.CommonSettings>
                        <@row>
                            <@cell columns=6>
                                <@form method="post" id="scriptWebsiteForm" onsubmit="saveRedirect()">
                                    <@field type="select" name="webSiteId" label="WebSite" size="30" name="scriptWebsiteId" id="scriptWebsiteId" required=true events={"change":"openSection(this.value);"}>
                                        <#list websites as website>
                                            <option value="${(website.webSiteId)!}"<#if parameters.webSiteId?has_content && parameters.webSiteId==website.webSiteId> selected="selected"</#if>>${(website.siteName)!(website.webSiteId)!}</option>
                                        </#list>
                                    </@field>
                                    <input type="hidden" id="row_index" name="row_index" value=""/>
                                    <@field type="text" id="row_from" name="from" value="" label="From" placeholder="a.html"/>
                                    <@field type="text" id="row_to" name="to" value="" label="To" placeholder="b.html"/>
                                    <@field type="select" id="row_type" name="type" label="Type">
                                        <option value="301">301 - Moved Permanently</option>
                                        <option value="308">308 - Permanent Redirect</option>
                                        <option value="302">302 - Temporary Redirect</option>
                                        <option value="303">303 - See Other</option>
                                        <option value="307">307 - Temporary Redirect (method and body not changed)</option>
                                        <option value="300">300 - Multiple Choice (use with rel=alternate)</option>
                                    </@field>
                                    <@field type="submit" text=uiLabelMap.CommonSave class="+${styles.link_run_sys!} ${styles.action_add!}"/>
                                </@form>
                            </@cell>
                        </@row>
                        <#list websites as website>
                                <div id="edit_${website.webSiteId}" class="wb_entry" <#if !parameters.webSiteId?has_content || parameters.webSiteId != website.webSiteId>style="display:none"</#if>>
                                    <@section title=website.siteName!website.webSiteId!"">
                                        <@row>
                                        <@cell columns=12>
                                            <@table id="table_${website.webSiteId}" type="data-list" responsive=false>
                                                <@thead>
                                                    <@tr>

                                                        <@th >${uiLabelMap.CommonFrom}</@th>
                                                        <@th >${uiLabelMap.CommonTo}</@th>
                                                        <@th >${uiLabelMap.CommonType}</@th>
                                                        <@th width="50"></@th>
                                                        <@th width="50"></@th>
                                                    </@tr>
                                                </@thead>
                                            </@table>
                                        </@cell>
                                    </@row>
                                </@section>
                            </div>
                        </#list>
                </@section>
            </@cell>
        </@row>
    </@section>
<#else>
    <@alert type="warming">No website entity found.</@alert>
</#if>


