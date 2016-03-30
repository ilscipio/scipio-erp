<#if prodCatalogId?has_content>
    <@section title=sectionTitle>    
        <form method="post" action="<@ofbizUrl>addProdCatalogToParty</@ofbizUrl>" name="AddProdCatalogToParty">    
            <input type="hidden" name="prodCatalogId" value="${prodCatalogId}"/>
            <@row>
                <@cell columns=12>
                    <@field type="lookup" id="partyId" name="partyId" label=uiLabelMap.CommonParty required=true formName="AddProdCatalogToParty" fieldFormName="LookupPartyName" value=((requestParameters.partyId)!)/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="select" label=uiLabelMap.PartyRole name="roleTypeId" size="1" required=true>
                        <#assign selectedKey = "">
                        <#list roleTypes as roleType>
                            <#if requestParameters.roleTypeId?has_content>
                                <#assign selectedKey = requestParameters.roleTypeId>                               
                            </#if>                           
                            <option <#if selectedKey == (roleType.roleTypeId!)> selected="selected"</#if> value="${roleType.roleTypeId}">${roleType.get("description",locale)}</option>
                        </#list>
                    </@field>
                </@cell>
            </@row>
             <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonFrom required=true name="fromDate" value=((requestParameters.fromDate)!) size="25" maxlength="30" id="fromDate1"/>
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="datetime" label=uiLabelMap.CommonThru name="thruDate" value="" size="25" maxlength="30" id="fromDate2" value=((requestParameters.thruDate)!)/>                      
                </@cell>
            </@row>
            <@row>
                <@cell columns=12>
                    <@field type="input" name="sequenceNum" label=uiLabelMap.CommonSequenceNum value=((requestParameters.sequenceNum)!) size=20 maxlength=40 />
                </@cell>
            </@row>

            <@row>
                <@cell>
                    <@field type="submit" name="Add" text=uiLabelMap.CommonAdd class="+${styles.link_run_sys!} ${styles.action_update!}"/>
                </@cell>
            </@row>            
        </form>
    </@section>
</#if>