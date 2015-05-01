<#--
Licensed to the Apache Software Foundation (ASF) under one
or more contributor license agreements.  See the NOTICE file
distributed with this work for additional information
regarding copyright ownership.  The ASF licenses this file
to you under the Apache License, Version 2.0 (the
"License"); you may not use this file except in compliance
with the License.  You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing,
software distributed under the License is distributed on an
"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
KIND, either express or implied.  See the License for the
specific language governing permissions and limitations
under the License.
-->

<#--
    <div id="footer">
        <ul>
            <li class="first">${nowTimestamp?datetime?string.short} - <a href="<@ofbizUrl>ListTimezones</@ofbizUrl>">${timeZone.getDisplayName(timeZone.useDaylightTime(), Static["java.util.TimeZone"].LONG, locale)}</a></li>
            <li><a href="<@ofbizUrl>ListLocales</@ofbizUrl>">${locale.getDisplayName(locale)}</a></li>
            <li class="last"><a href="<@ofbizUrl>ListVisualThemes</@ofbizUrl>">${uiLabelMap.CommonVisualThemes}</a></li>
        </ul>
      <p>
      ${uiLabelMap.CommonCopyright} (c) 2001-${nowTimestamp?string("yyyy")} <a href="http://www.apache.org" target="_blank">The Apache Software Foundation</a>. ${uiLabelMap.CommonPoweredBy} <a href="http://ofbiz.apache.org" target="_blank">Apache OFBiz.</a> <#include "ofbizhome://runtime/svninfo.ftl" /> <#include "ofbizhome://runtime/gitinfo.ftl" />
      </p>
    </div>
    </div>
-->
</section>
</div></div>
<!-- FOOTER SECTION -->
<footer id="footer">
    <#--
    <div class="row"> 
        <div class="small-12 medium-4 large-4 columns">
            <p></p>
        </div>
    </div>-->
    <div class="row">
        <div class="small-12 medium-12 large-12 columns text-center">
         <small>
         ${uiLabelMap.CommonCopyright} (c) 2001-${nowTimestamp?string("yyyy")} <a href="http://www.apache.org" target="_blank">The Apache Software Foundation</a>. ${uiLabelMap.CommonPoweredBy} <a href="http://ofbiz.apache.org" target="_blank">Apache OFBiz.</a>
         <#include "ofbizhome://runtime/svninfo.ftl" /> <#include "ofbizhome://runtime/gitinfo.ftl" />
         </small>
        </div>
    </div>
</footer> <!-- END FOOTER -->


    <#if layoutSettings.VT_FTR_JAVASCRIPT?has_content>
        <#--layoutSettings.javaScripts is a list of java scripts. -->
        <#-- use a Set to make sure each javascript is declared only once, but iterate the list to maintain the correct order -->
        <#assign javaScriptsSet = Static["org.ofbiz.base.util.UtilMisc"].toSet(layoutSettings.VT_FTR_JAVASCRIPT)/>
        <#list layoutSettings.VT_FTR_JAVASCRIPT as javaScript>
            <#if javaScriptsSet.contains(javaScript)>
                <#assign nothing = javaScriptsSet.remove(javaScript)/>
                <script src="<@ofbizContentUrl>${StringUtil.wrapString(javaScript)}</@ofbizContentUrl>" type="text/javascript"></script>
            </#if>
        </#list>
    </#if>
</body>
</html>
