<form name="demoDataGeneratorForm" method="post" action="<@ofbizUrl>DemoDataGeneratorResult?_RUN_SYNC_=Y</@ofbizUrl>">
      <#assign serviceParameterNames = [] />

      <#-- Scipio: leave room for the label area because service parameter names can be long -->
      <@fields fieldArgs={"labelColumns":4}>
        <#list serviceParameters as serviceParameter>    
          <#if serviceParameter.type == "Boolean">
            <#assign value = "N" />
            <#if serviceParameter.defaultValue?has_content><#if (serviceParameter.defaultValue)><#assign value = "Y" /></#if></#if>             
            <#assign fieldLabel>${rawString(serviceParameter.name)} (<em>${rawString(serviceParameter.type)}</em>)</#assign>
            <@field type="checkbox" label=fieldLabel name=serviceParameter.name value=value required=(serviceParameter.optional == "N") checked=(value == "Y") />
          <#elseif serviceParameter.type == "Timestamp">
            <#assign fieldLabel>${rawString(serviceParameter.name)} (<em>${rawString(serviceParameter.type)}</em>)</#assign>
            <@field type="datetime" label=fieldLabel name=serviceParameter.name value=(serviceParameter.value!) required=(serviceParameter.optional == "N") />
          <#else>
            <#assign fieldLabel>${rawString(serviceParameter.name)} (<em>${rawString(serviceParameter.type)}</em>)<#if serviceParameter.defaultValue?has_content> (${rawString(uiLabelMap.WebtoolsServiceDefault)}: <em>${rawString(serviceParameter.defaultValue)}</em>)</#if></#assign>
            <@field type="input" label=fieldLabel size="20" name=serviceParameter.name value=(serviceParameter.value!) required=(serviceParameter.optional == "N")/>
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