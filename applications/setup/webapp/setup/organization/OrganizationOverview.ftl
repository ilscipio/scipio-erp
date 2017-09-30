<#include "component://setup/webapp/setup/common/common.ftl">
  

<hr/>

<#assign htmlSectionTitle>${uiLabelMap.CommonOverview}<#rt/>
  <#lt/> (<@setupExtAppLink uri="/partymgr/control/viewprofile?partyId=${rawString(partyId!)}" text=uiLabelMap.CommonManage/>)</#assign>
<#assign sectionTitle = wrapAsRaw({"htmlmarkup":htmlSectionTitle, "raw":rawLabel('PartyContactInformation')})>
      
<@section title=sectionTitle relHeadingLevel=+1>

  <#-- not sure this adds anything
  <@render name="Party" resource="component://party/widget/partymgr/ProfileScreens.xml"/>-->

  <#macro menuContent menuArgs={}>
    <@menu args=menuArgs>
        <@menuitem type="link" href=makeOfbizUrl("editcontactmech?partyId=${rawString(partyId)}") text=uiLabelMap.CommonNew class="+${styles.action_nav!} ${styles.action_add!}"/>
    </@menu>
  </#macro>
  <@section title=uiLabelMap.PartyContactInformation menuContent=menuContent>
    <@render name="Contact" resource="component://party/widget/partymgr/ProfileScreens.xml"/>
  </@section>

</@section>