<@section>
    <form name="MyCommunicationEvents" action="<@ofbizUrl>MyCommunicationEvents</@ofbizUrl>" method="POST">
    
        <@field type="lookup" name="partyIdTo" formName="MyCommunicationEvents" id="partyIdTo" fieldFormName="LookupPartyName" label=uiLabelMap.PartyPartyTo />
    
        <@field type="select" name="comEventStatus" label=uiLabelMap.MarketingCommunicationStatusId>
            <option value=""></value>
            <#list comEventStatusList as comEventStatus>
                <option value="${comEventStatus.statusId}">${comEventStatus.description}</option>
            </#list>
       </@field>

        <@field type="select" name="comEventRoleStatus" label=uiLabelMap.MarketingCommunicationRoleStatusId>
            <option value=""></value>   
            <#list comEventRoleStatusList as comEventRoleStatus>
                <option value="${comEventRoleStatus.statusId}">${comEventRoleStatus.description}</option>
            </#list>
       </@field>

       <@field type="select" name="communicationEventTypeId" label=uiLabelMap.MarketingContactListCommEventTypeId>
            <option value=""></value>           
            <#list communicationEventTypeList as communicationEventType>
                <option value="${communicationEventType.communicationEventTypeId}">${communicationEventType.description}</option>
            </#list>
       </@field>

       <@field type="submit" name="find" text=uiLabelMap.CommonFind class="+${styles.link_run_sys!} ${styles.action_find!}" />
    </form>
</@section>