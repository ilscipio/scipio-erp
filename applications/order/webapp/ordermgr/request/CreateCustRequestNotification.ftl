<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->
<h1>${title}</h1>
<p>Hello ${person.firstName!} ${person.middleName!} ${person.lastName!},</p>
<p>Your Customer Request ${custRequest.custRequestName!} [${custRequest.custRequestId}] has been created successfully.
<br /><br />
We will solve/implement the request as soon as possible
<br /><br />
The status and used hours can always be checked <br />
<a href="${rawString(baseSecureUrl!)}/myportal/control/showPortletMainDecorator?portalPortletId=ViewCustRequest&amp;id=${custRequest.custRequestId}">here....</a>
<br /><br />
Regards.
<br /><br />
PS. we will notify you when the customer request is completed.
</p>
