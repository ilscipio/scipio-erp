<form name="demoDataGeneratorForm" method="post" action="<@ofbizUrl>DemoDataGeneratorResult?_RUN_SYNC_=Y</@ofbizUrl>">
      <#assign serviceParameterNames = [] />

      <#-- SCIPIO: leave room for the label area because service parameter names can be long -->
      <@fields fieldArgs={"labelColumns":4}>
        <#list serviceParameters as serviceParameter>    
          <#if serviceParameter.type == "Boolean">
            <#assign value = "false" />
            <#if serviceParameter.defaultValue?has_content><#if (serviceParameter.defaultValue)><#assign value = "true" /></#if></#if>             
            <#assign fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)</#assign>
            <@field type="checkbox" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name=serviceParameter.name currentValue=value value="true" altValue="false" required=(serviceParameter.optional == "N") />
          <#elseif serviceParameter.type == "Timestamp">
            <#assign fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)</#assign>
            <@field type="datetime" label=wrapAsRaw(fieldLabel, 'htmlmarkup') name=serviceParameter.name value=(serviceParameter.value!) required=(serviceParameter.optional == "N") />
          <#else>
            <#assign fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if serviceParameter.defaultValue!?string?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${serviceParameter.defaultValue?string}</em>)</#if></#assign>
            <@field type="input" label=wrapAsRaw(fieldLabel, 'htmlmarkup') size="20" name=serviceParameter.name value=(serviceParameter.value!) required=(serviceParameter.optional == "N")/>
          </#if>
          <#assign serviceParameterNames = serviceParameterNames + [serviceParameter.name] />
        </#list>
      </@fields>
      <#list scheduleOptions as scheduleOption>
         <#if !serviceParameterNames?seq_contains(scheduleOption.name)>
            <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
         </#if>
      </#list>

      <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_begin!}" />
</form>