<div id="cms-pgver-list-area">
<@script>
    function cmsPgVerUpdate() {
        //var versionId = "${escapeVal(versionId, 'js')}";
        var dt = $('#cms-pgver-list-table').dataTable({'retrieve': true});
        if (dt != null && typeof dt.api === 'function') { dt = dt.api(); }
        if (dt != null && typeof dt.draw === 'function') {
            dt.rows().remove();
            $('#cms-pgver-list-body-tgt tr').each(function() {
                var cells = [];
                $('td', this).each(function() {
                    cells.push($(this).html());
                });
                dt.rows.add([cells]);
            });
            dt.draw(false); <#-- draw first to ensure the new tr elements are created -->
            var i = 0;
            $('#cms-pgver-list-body-tgt tr').each(function() {
                <#-- Can't do this, the even/row get mixed up:
                $(dt.row(i).node()).addClass($(this).attr('class'));-->
                if ($(this).hasClass("${escapeVal(styles.row_selected!, 'js')}")) {
                    $(dt.row(i).node()).addClass("${escapeVal(styles.row_selected!, 'js')}");
                }
                i++;
            });
        } else {
            console.log('missing datatables .api().draw() for #cms-pgver-list-table');
        }
    };
</@script>
<div id="cms-pgver-list-body-tgt" style="display:none;"></div>
<@paginate mode="content" layout="bottom" ajaxEnabled=true areaId=["#cms-pgver-list-body::#cms-pgver-list-body-tgt", "#cms-pgver-list-area .pagination-area"] areaCallback="cmsPgVerUpdate"
    url=makePageUrl("pageVersionList") paramStr=("pageId="+raw(pageId!)+"&versionId="+raw(versionId!)) paramPrefix="pageVer_" altParam=true
    viewIndex=((pageVersions.viewIndex)!0) listSize=((pageVersions.listSize)!0) viewSize=((pageVersions.viewSize)!1)>
    <@table id="cms-pgver-list-table" type="data-complex" autoAltRows=true responsive=true scrollable=true fixedColumnsRight=1>
        <@thead>
            <@tr>
                <@th width="32px"></@th>
                <@th width="200px"></@th>
                <@th width="200px"></@th>
                <@th></@th>
                <@th width="32px"></@th>
                <@th width="32px"></@th>
            </@tr>
        </@thead>
        <@tbody id="cms-pgver-list-body">
            <#list ((pageVersions.data)!) as version>
                <#assign curVersionId = raw(version.id)>
                <#assign rowSelected = (raw(versionId) == curVersionId)>
                <@tr alt=rowSelected selected=rowSelected>
                   <@td><i class="${styles.text_color_info} ${styles.icon!} ${styles.icon_user!}" style="font-size:16px;margin:4px;"></i></@td>
                   <@td>${version.createdBy!"Anonymous"}</@td>
                   <#assign verLinkMkrp><@pageUrl escapeAs="html">editPage?pageId=${escapeVal(pageId!, 'url')}&versionId=${escapeVal(curVersionId, 'url')}</@pageUrl></#assign>
                   <@td><#if version.lastModified?has_content><a href="${verLinkMkrp}">${raw(version.lastModified)?datetime}</a></#if></@td>
                   <@td><#if version.versionComment?has_content>${version.versionComment!""}</#if></@td>
                   <@td><a href="${verLinkMkrp}"><i class="${styles.text_color_info} ${styles.icon!} ${styles.icon_edit!}" style="font-size:16px;margin:4px;"></i></a></@td>
                   <@td><#if curVersionId == raw(activeVersionId!)><i class="${styles.text_color_success} ${styles.icon!} ${styles.icon_check!}" style="font-size:16px;margin:4px;"></i></#if></@td>
                </@tr>
            </#list>
        </@tbody>
    </@table>
</@paginate>
</div>
