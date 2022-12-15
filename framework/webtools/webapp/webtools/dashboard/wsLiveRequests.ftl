<#assign library = chartLibrary!"foundation"/>
<#assign currData = rewrapMap(chartData, "raw-simple")/>
<#assign fieldIdNum = fieldIdNum!0/>

<@script>
    var maxRequestsEntries = ${maxRequestsEntries};
    function setMaxRequestsEntries(val) { val = parseInt(val); if (!isNaN(val)) { maxRequestsEntries = val; console.log("setting maxRequestsEntries: " + maxRequestsEntries); } }

      $(function(){
            var webSocket = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/requestdatalive/subscribe</@appUrl>');
            webSocket.onopen = function(event){
                var msg = {};
              webSocket.send(JSON.stringify(msg));
              <#-- disabled: this now seems counterproductive because all it does is add empty space and the 10 seconds no longer matches anything server-side
              setTimeout(function() {
                    setInterval(timedUpdater, 10000); <#- - Run every 10 seconds - ->
              }, ((60 - new Date().getSeconds()) * 1000)); <#- - Start after 10s - ->
              -->
            };

            webSocket.onmessage = function(event) {
                 var jsonObject, message;
                 var text = event.data;
                 message = text;
                try {
                      jsonObject =  JSON.parse(text);
                      var chart = $('#requestchart').data("chart");
                      //var hasData = false;
                      $.each(jsonObject, function (key, value) {
                          var curIndex = chart.data.labels.indexOf(key)
                          if (curIndex > -1) {
                             chart.data.datasets[0].data[curIndex] = jsonObject[key].count;
                          } else {
                             if (chart.data.labels.length >= maxRequestsEntries) {
                                 <#-- Remove old data and add new -->
                                 chart.data.labels.shift();
                                 chart.data.datasets[0].data.shift();
                             }
                             <#-- Add new element -->
                             chart.data.labels.push(key);
                             chart.data.datasets[0].data.push(jsonObject[key].count);
                          }
                          //hasData = true;
                      });
                      //if (hasData) {
                      chart.update();
                      //}
                    } catch (error) {
                        console.log(error);
                    }
            };
            <#--
            function timedUpdater() {
                var date = new Date();
                var time = moment("YYYY-MM-DD'T'HH:mm");
                var chart = $('#requestchart').data("chart");
                var curIndex = chart.data.labels.indexOf(time)
                if (curIndex == -1) {
                     chart.data.labels.push(time);
                     chart.data.datasets[0].data.push(0);
                }
                return false;
            }
            -->
      });
</@script>

<@section title=title!"">
<#--<#if currData?has_content>-->
    <#if chartType == "line" || chartType == "bar">
        <@chart id="requestchart" type=chartType xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"")>
            <#-- Additional chart options -->
            try {
                chart.config.options.scales.xAxes[0].ticks.display=false;
            } catch(e) {

            }
            <#-- Chart data -->
            <#list mapKeys(currData!) as key>
                <#assign date = key?date/>
                <@chartdata value=((currData[key].count)!0) title=key/>
            </#list>
        </@chart>
    <#elseif chartType == "pie">
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    <#else>
        <@commonMsg type="error">${uiLabelMap.CommonUnsupported}</@commonMsg>
    </#if>
<#--<#else>
    <@commonMsg type="result-norecord"/>
</#if>-->
</@section>

<@field type="text" name="maxRequestsEntries" value=maxRequestsEntries label=uiLabelMap.CommonMaxEntries
events={"change":"setMaxRequestsEntries($(this).val());", "keyup":"setMaxRequestsEntries($(this).val());"}/>