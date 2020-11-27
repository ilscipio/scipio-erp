<@section title=title>
<@table id="global-job-stats-${jobType}" type="data-list" responsive=true>
    <@thead>
        <@tr>
            <@th>serviceName</@th>
            <@th width="8%">totalCalls</@th>
            <@th width="8%">totalRuntime</@th>
            <@th width="8%">minRuntime</@th>
            <@th width="8%">maxRuntime</@th>
            <@th width="8%">averageRuntime</@th>
            <@th width="8%">successCount</@th>
            <@th width="8%">failCount</@th>
            <@th width="8%">errorCount</@th>
            <@th width="8%">exceptionCount</@th>
        </@tr>
    </@thead>
    <@tbody>
        <#if jobList?has_content>
            <#list (jobList!) as job>
                <@tr>
                    <@td><#if job.serviceName?has_content><a href="<@pageUrl uri='ServiceList?sel_service_name='+raw(job.serviceName)/>">${job.serviceName}</a></#if></@td>
                    <@td width="8%">${job.totalCalls!}</@td>
                    <@td width="8%">${job.totalRuntime!}</@td>
                    <@td width="8%">${job.minRuntime!}</@td>
                    <@td width="8%">${job.maxRuntime!}</@td>
                    <@td width="8%">${job.averageRuntime!}</@td>
                    <@td width="8%">${job.successCount!}</@td>
                    <@td width="8%">${job.failCount!}</@td>
                    <@td width="8%">${job.errorCount!}</@td>
                    <@td width="8%">${job.exceptionCount!}</@td>
                </@tr>
            </#list>
        <#--<#else> let datatables do it or it crashes
            <@tr><@td colspan="10">${getLabel('CommonNone', 'CommonUiLabels')}</@td></@tr>-->
        </#if>
    </@tbody>
</@table>
</@section>