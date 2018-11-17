<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<@section>
        <#-- checkoutsetupform is used for the order entry "continue" link -->
        <form method="post" action="<@ofbizUrl>finalizeOrder</@ofbizUrl>" name="checkoutsetupform">
            <input type="hidden" name="finalizeMode" value="term" />
        </form>
        <@row>
            <@cell columns=6>
                        <#if orderTerms?has_content && parameters.createNew?default('') != 'Y'>
                            <@table type="data-list" autoAltRows=true>
                              <@thead>
                                <@tr class="header-row">
                                  <@th>${uiLabelMap.OrderOrderTermType}</@th>
                                  <@th align="center">${uiLabelMap.OrderOrderTermValue}</@th>
                                  <@th align="center">${uiLabelMap.OrderOrderTermDays}</@th>
                                  <@th align="center">${uiLabelMap.OrderOrderTextValue}</@th>
                                  <@th>${uiLabelMap.CommonDescription}</@th>
                                  <@th>&nbsp;</@th>
                                </@tr>
                              </@thead>
                              <@tbody>
                                <#list orderTerms as orderTerm>
                                    <@tr>
                                        <@td nowrap="nowrap">${orderTerm.getRelatedOne('TermType', false).get('description', locale)}</@td>
                                        <@td align="center">${orderTerm.termValue!}</@td>
                                        <@td align="center">${orderTerm.termDays!}</@td>
                                        <@td nowrap="nowrap">${orderTerm.textValue!}</@td>
                                        <@td nowrap="nowrap">${orderTerm.description!}</@td>
                                        <@td align="right">
                                            <a href="<@ofbizUrl>setOrderTerm?termIndex=${orderTerm_index}&amp;createNew=Y</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_update!}">${uiLabelMap.CommonUpdate}</a>
                                            <a href="<@ofbizUrl>removeCartOrderTerm?termIndex=${orderTerm_index}</@ofbizUrl>" class="${styles.link_run_session!} ${styles.action_remove!}">${uiLabelMap.CommonRemove}</a>
                                        </@td>
                                    </@tr>
                                </#list>
                              </@tbody>
                              <@tfoot>
                                <@tr>
                                    <@td colspan="5">
                                      <a href="<@ofbizUrl>setOrderTerm?createNew=Y</@ofbizUrl>" class="${styles.link_nav!} ${styles.action_add!}">${uiLabelMap.CommonNew}</a>
                                    </@td>
                                </@tr>
                              </@tfoot>
                            </@table>
                        <#else>
                            <form method="post" action="<@ofbizUrl>addOrderTerm</@ofbizUrl>" name="termform">
                                <input type="hidden" name="termIndex" value="${termIndex!}" />
                                    <@field type="select" label=uiLabelMap.OrderOrderTermType name="termTypeId">
                                            <option value=""></option>
                                            <#list termTypes! as termType>
                                                <option value="${termType.termTypeId}"
                                                    <#if termTypeId?default('') == termType.termTypeId>selected="selected"</#if>
                                                >${termType.get('description', locale)}</option>
                                            </#list>
                                    </@field>
                                    <@field type="input" label=uiLabelMap.OrderOrderTermValue size="30" maxlength="60" name="termValue" value=(termValue!) />
                                    <@field type="input" label=uiLabelMap.OrderOrderTermDays size="30" maxlength="60" name="termDays" value=(termDays!) />
                                    <@field type="input" label=uiLabelMap.OrderOrderTextValue size="30" maxlength="60" name="textValue" value=(textValue!) />
                                    <@field type="input" label=uiLabelMap.CommonDescription size="30" maxlength="255" name="description" value=(description!) />
                                    <@field type="submit" class="+${styles.link_run_sys!} ${styles.action_add!}" text=uiLabelMap.CommonAdd />
                            </form>
                        </#if>
            </@cell>
        </@row>
</@section>