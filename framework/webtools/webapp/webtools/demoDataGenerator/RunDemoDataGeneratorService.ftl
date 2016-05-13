<form name="demoDataGeneratorForm" method="post" action="<@ofbizUrl>DemoDataGeneratorResult?_RUN_SYNC_=Y</@ofbizUrl>">

    <#list scheduleOptions as scheduleOption>
      <input type="hidden" name="${scheduleOption.name}" value="${scheduleOption.value}"/>
    </#list>

      <#-- Cato: leave room for the label area because service parameter names can be long -->
      <@fields fieldArgs={"labelColumns":4}>
        <#list serviceParameters as serviceParameter>    
          <#if serviceParameter.type == "Boolean">
            <#assign value = "N" />
            <#if serviceParameter.defaultValue?has_content><#if (serviceParameter.defaultValue)><#assign value = "Y" /></#if></#if>             
            <#assign fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)</#assign>
            <@field type="checkbox" label=fieldLabel name="${serviceParameter.name}" value=value required=(serviceParameter.optional == "N") checked=(value == "Y") />
          <#elseif serviceParameter.type == "Timestamp">
            <#assign fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)</#assign>
            <@field type="datetime" label=fieldLabel name="${serviceParameter.name}" value=(serviceParameter.value!) required=(serviceParameter.optional == "N") />
          <#else>
            <#assign fieldLabel>${serviceParameter.name} (<em>${serviceParameter.type}</em>)<#if serviceParameter.defaultValue?has_content> (${uiLabelMap.WebtoolsServiceDefault}: <em>${serviceParameter.defaultValue?string}</em>)</#if></#assign>
            <@field type="input" label=fieldLabel size="20" name="${serviceParameter.name}" value=(serviceParameter.value!) required=(serviceParameter.optional == "N")/>
          </#if>
          
        </#list>
      </@fields>

    <@field type="submit" text=uiLabelMap.CommonSubmit class="${styles.link_run_sys!} ${styles.action_begin!}" />

</form>