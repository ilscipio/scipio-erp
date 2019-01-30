<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->

<h1>${title}</h1>
<p>Hello ${person.firstName!} ${person.lastName!},</p>
<p>Your Customer Request ${custRequest.custRequestName!} [${custRequest.custRequestId}] has been completed.
<br /><br />
The status and used hours can always be checked at the url: <br />
<a href="<@serverUrl>/myportal/control/showPortletMainDecorator?portalPortletId=ViewCustRequest&amp;id=${custRequest.custRequestId}</@serverUrl>"><@serverUrl>/myportal/control/ViewRequest?custRequestId=${custRequest.custRequestId}</@serverUrl></a>
<br /><br />
Regards.<br /><br />
Thank you for your business.
