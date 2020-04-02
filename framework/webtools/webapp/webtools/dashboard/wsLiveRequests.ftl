<#assign library=chartLibrary!"foundation"/>
<#assign currData=rewrapMap(chartData, "raw-simple")/>
<#assign fieldIdNum=fieldIdNum!0/>

<@script>
      $(function(){
            var webSocket = new WebSocket('wss://' + window.location.host + '<@appUrl fullPath="false">/ws/requestdatalive/subscribe</@appUrl>');
            webSocket.onopen = function(event){
                var msg = {};
              webSocket.send(JSON.stringify(msg));
              setTimeout(function() {
                    setInterval(timedUpdater, 10000); <#-- Run every 10 seconds -->
              }, ((60 - new Date().getSeconds()) * 1000)); <#-- Start after 10s -->
            };

            webSocket.onmessage = function(event) {
                 var jsonObject, message;
                 var text = event.data;
                 message = text;
                  try {
                      jsonObject =  JSON.parse(text);
                      var chart = requestchart;

                      $.each(jsonObject, function (key, value) {
                          var curIndex = requestchart.data.labels.indexOf(key)
                          if (curIndex > -1) {
                             chart.data.datasets[0].data[curIndex] = jsonObject[key].count;
                          } else {
                             <#-- Remove old data and add new -->
                             chart.data.labels.shift();
                             chart.data.datasets[0].data.shift();
                             <#-- Add new element -->
                             chart.data.labels.push(key);
                             chart.data.datasets[0].data.push(jsonObject[key].count);
                          }
                      });

                      chart.update();
                    } catch (error) {
                        console.log(error);
                    }
            };

            function timedUpdater() {
                var date = new Date();
                var time = moment("YYYY-MM-DD'T'HH:mm");
                var curIndex = requestchart.data.labels.indexOf(time)
                if (curIndex == -1) {
                     var chart = requestchart;
                     chart.data.labels.push(time);
                     chart.data.datasets[0].data.push(0);
                }
                return false;
            }
      });
</@script>

<#if title?has_content><@heading relLevel=1>${title}</@heading></#if>
<#--<#if currData?has_content>-->
    <#if chartType == "line" || chartType == "bar">
        <@chart id="requestchart" type=chartType xlabel=(xlabel!"") ylabel=(ylabel!"") label1=(label1!"") label2=(label2!"")>
            <#-- Additional chart options -->
            try{
                requestchart.config.options.scales.xAxes[0].ticks.display=false;
            }catch(e){

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
