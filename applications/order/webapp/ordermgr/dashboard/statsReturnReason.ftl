<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<#assign chartType=chartType!"doughnut"/>    <#-- (line|bar|pie) default: line -->
<#assign chartData=chartData!"month"/>
<#assign library=chartLibrary!"chart"/>
<#assign xlabel=xlabel!""/>
<#assign ylabel=ylabel!""/>
<#assign label1=label1!""/>
<#assign label2=label2!""/>
<#-- <#assign chartDataMap={"day":dailyStats,"week":weeklyStats,"month":monthlyStats}/>
<#assign currData=chartDataMap[chartData]/> -->
<#assign currData=rewrapMap(orderStats!{}, "raw-simple")/>
<#assign fieldIdNum=fieldIdNum!0/>
<#-- Return reasons -->
<@script>
$(function(){
Chart.pluginService.register({
  beforeDraw: function(chart) {
    if (chart.config.options.elements.center) {
      // Get ctx from string
      var ctx = chart.chart.ctx;

      // Get options from the center object in options
      var centerConfig = chart.config.options.elements.center;
      var fontStyle = centerConfig.fontStyle || 'Arial';
      var txt = centerConfig.text;
      var color = centerConfig.color || '#000';
      var maxFontSize = centerConfig.maxFontSize || 75;
      var sidePadding = centerConfig.sidePadding || 20;
      var sidePaddingCalculated = (sidePadding / 100) * (chart.innerRadius * 2)
      // Start with a base font of 30px
      ctx.font = "30px " + fontStyle;

      // Get the width of the string and also the width of the element minus 10 to give it 5px side padding
      var stringWidth = ctx.measureText(txt).width;
      var elementWidth = (chart.innerRadius * 2) - sidePaddingCalculated;

      // Find out how much the font can grow in width.
      var widthRatio = elementWidth / stringWidth;
      var newFontSize = Math.floor(30 * widthRatio);
      var elementHeight = (chart.innerRadius * 2);

      // Pick a new font size so it will not be larger than the height of label.
      var fontSizeToUse = Math.min(newFontSize, elementHeight, maxFontSize);
      var minFontSize = centerConfig.minFontSize;
      var lineHeight = centerConfig.lineHeight || 25;
      var wrapText = false;

      if (minFontSize === undefined) {
        minFontSize = 20;
      }

      if (minFontSize && fontSizeToUse < minFontSize) {
        fontSizeToUse = minFontSize;
        wrapText = true;
      }

      // Set font settings to draw it correctly.
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      var centerX = ((chart.chartArea.left + chart.chartArea.right) / 2);
      var centerY = ((chart.chartArea.top + chart.chartArea.bottom) / 2);
      ctx.font = fontSizeToUse + "px " + fontStyle;
      ctx.fillStyle = color;

      if (!wrapText) {
        ctx.fillText(txt, centerX, centerY);
        return;
      }

      var words = txt.split(' ');
      var line = '';
      var lines = [];

      // Break words up into multiple lines if necessary
      for (var n = 0; n < words.length; n++) {
        var testLine = line + words[n] + ' ';
        var metrics = ctx.measureText(testLine);
        var testWidth = metrics.width;
        if (testWidth > elementWidth && n > 0) {
          lines.push(line);
          line = words[n] + ' ';
        } else {
          line = testLine;
        }
      }

      // Move the center up depending on line height and number of lines
      centerY -= (lines.length / 2) * lineHeight;

      for (var n = 0; n < lines.length; n++) {
        ctx.fillText(lines[n], centerX, centerY);
        centerY += lineHeight;
      }
      //Draw text in center
      ctx.fillText(line, centerX, centerY);
    }
  }
});

});


</@script>

<@section title=title!"">
  <@chart type=chartType library=library xlabel=xlabel ylabel=ylabel label1=label1 label2=label2 id="retrchart">
    <#assign retTotal=0/>
    <#if currData?has_content>
        <#list currData as obj>
          <#assign reasonId = obj["returnReasonId"]/>
          <#assign returnReasonDescr = returnReasonMap[reasonId] />
          <#assign retTotal=retTotal+(obj["totalQuantity"])!0/>
          <@chartdata value=(obj["totalQuantity"]) title=returnReasonDescr/>
        </#list>
    </#if>

    config.options.elements = {
        center : {
            display: true,
            text:'${retTotal!0} returns',
            color: '#333', // Default is #000000
            sidePadding: 40, // Default is 20 (as a percentage)
            minFontSize: 10, // Default is 20 (in px), set to false and text will not wrap.
            lineHeight: 15 // Default is 25 (in px), used for when text wraps
        }
    };
    config.options.legend = {
        position: 'right',
        align: 'left',
        labels: {
          boxWidth: 10,
          fontSize: 12
        }
    };
  </@chart>
</@section>