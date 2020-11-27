
<@section title=title>
<#if !((coreOnly!"")?is_boolean)>
    <#assign coreOnly = (parameters.coreOnly!"false")?boolean>
</#if>
<#if !coreOnly>
<@script>
    <#assign currentJobStatsInverval = (parameters.currentJobStatsInverval!"5000")?number>
    var currentJobStatsInterval = {
        tableId : 'current-job-stats',
        interval : ${currentJobStatsInverval},
        timer : null,
        enabled : true,
        updateInterval : function(interval) {
            if (interval != null) {
                interval = parseInt(interval);
                if (isNaN(interval)) {
                    return;
                }
                this.interval = interval;
            } else {
                interval = this.interval;
                if (isNaN(interval)) {
                    return;
                }
            }
            if (this.timer != null) {
                clearInterval(this.timer);
                this.timer = null;
            }
            var tableId = this.tableId;
            var obj = this;
            this.timer = setInterval(function() {
                if (obj.enabled) {
                    obj.reload();
                }
            }, interval);
        },
        reload : function() {
            var tableId = this.tableId;
            $.ajax({
                url : "<@pageUrl uri='currentJobStats' escapeAs='js'/>",
                method: 'POST',
                data: { 'coreOnly' : true }
            }).done(function(data) {
                $('#'+tableId).html($(data).find('#'+tableId).parent().html());
            });
        },
        toggle : function() {
            if (this.enabled) {
                this.enabled = false;
                if (this.timer != null) {
                    clearInterval(this.timer);
                    this.timer = null;
                }
            } else {
                this.updateInterval();
                this.enabled = true;
            }
        }
    }
    $(document).ready(function() {
        currentJobStatsInterval.reload();
        currentJobStatsInterval.updateInterval($('#current-job-stats-config input[name=currentJobStatsInterval]').val());
    });
    function updateCurrentJobStatsInterval(interval) {
        currentJobStatsInterval.updateInterval(interval);
    }
    function toggleCurrentJobStatsInterval() {
        var btn = $('#current-job-stats-toggle');
        currentJobStatsInterval.toggle();
        if (currentJobStatsInterval.enabled) {
            btn.removeClass("${escapeVal(styles.color_red!, 'js')}").addClass("${escapeVal(styles.color_green!, 'js')}");
            btn.text("Running");
        } else {
            btn.removeClass("${escapeVal(styles.color_green!, 'js')}").addClass("${escapeVal(styles.color_red!, 'js')}");
            btn.text("Stopped");
        }
    }
</@script>
<@form name="current-job-stats-config" id="current-job-stats-config">
  <#assign pauseMarkup>
    <@field type="submit" submitType="link" class="${styles.link_run_local!} ${styles.color_green!}" text="Running"
        href="javascript:toggleCurrentJobStatsInterval();" id="current-job-stats-toggle"/>
  </#assign>
    <@field type="text" name="currentJobStatsInterval" value=currentJobStatsInverval label=uiLabelMap.CommonTimeInterval
        events={"change":"updateCurrentJobStatsInterval($(this).val());", "keyup":"updateCurrentJobStatsInterval($(this).val());"}
        postfix=true postfixContent=pauseMarkup postfixColumns=1/>
</@form>
</#if>
<@table id="current-job-stats" type="data-list" responsive=false>
    <@thead>
        <@tr>
            <@th>serviceName</@th>
            <@th width="15%">jobName</@th>
            <@th width="15%">jobId</@th>
            <@th width="15%">jobType</@th>
            <@th width="15%">startTime</@th>
            <@th width="15%">runTime</@th>
        </@tr>
    </@thead>
    <@tbody>
        <#if jobList?has_content>
            <#list (jobList!) as job>
                <@tr>
                    <@td><#if job.serviceName?has_content><a href="<@pageUrl uri='ServiceList?sel_service_name='+raw(job.serviceName)/>">${job.serviceName}</a></#if></@td>
                    <@td width="15%">${job.jobName!}</@td>
                    <@td width="15%">${job.jobId!}</@td>
                    <@td width="15%">${job.jobType!}</@td>
                    <@td width="15%">${job.startTime!}</@td>
                    <@td width="15%">${job.runTime!}</@td>
                </@tr>
            </#list>
        <#else>
            <@tr><@td colspan="6">${getLabel('CommonNone', 'CommonUiLabels')}</@td></@tr>
        </#if>
    </@tbody>
</@table>
</@section>

<#-- FIXME: previous version: DataTables fails to extract the column data from the initial ajax request data,
    and then ajax.reload() refuses to run - api/version bug or coding issue?
<#assign responsiveOptions = {
    "fixedHeader" : true,
    "info" : true,
    "paging" : false,
    "searching" : true,
    "ordering" : true,
    "ajax" : {
        "url" : makePageUrl("currentJobStatsJson"),
        "type": "POST",
        "columns" : [
            {"data" : "jobName"},
            {"data" : "jobId"},
            {"data" : "startTime"},
            {"data" : "runTime"},
            {"data" : "serviceName"},
            {"data" : "jobType"}
        ]
    }
}>
<@script>
    // ... (see above)
    var currentJobStatsInterval = {
        // ... (see above)
        reload : function() {
            console.log("Reloading: " + this.tableId);
            var ajax = $(this.tableId).dataTable().api().ajax;
            ajax.reload(null, false);
        },
        // ... (see above)
    }
    // ... (see above)
</@script>
<@form name="current-job-stats-config" id="current-job-stats-config">
    <@field type="text" name="currentJobStatsInterval" value=currentJobStatsInverval label=uiLabelMap.CommonTimeInterval
        events={"change":"currentJobStatsInterval.updateInterval($(this).val());", "keyup":"currentJobStatsInterval.updateInterval($(this).val());"}/>
    <@field type="submit" submitType="link" href="javascript:currentJobStatsInterval.toggle()" class="${styles.link_run_local!} ${styles.action_add!}" text=uiLabelMap.CommonToggle />
</@form>
<@table id="current-job-stats" type="data-list" responsive=true responsiveOptions=responsiveOptions>
    <@thead>
        <@tr>
            <@th>jobName</@th>
            <@th>jobId</@th>
            <@th>startTime</@th>
            <@th>runTime</@th>
            <@th>serviceName</@th>
            <@th>jobType</@th>
        </@tr>
    </@thead>
    <@tbody>
    </@tbody>
</@table>
-->