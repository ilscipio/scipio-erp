<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
-->
<html xmlns="http://www.w3.org/1999/xhtml">
    <head>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <title>${title}</title>
        <link rel="stylesheet" href="<@contentUrl>/images/maincss.css</@contentUrl>" type="text/css"/>
    </head>
    <body>
        <h2>${title}</h2>
        <p>Hello ${person.firstName!} ${person.lastName!},</p>
        <p>Your Customer Request ${custRequest.custRequestName!} [${custRequest.custRequestId}] has a note added to it:<br />
        ${noteData.noteInfo!}
        <br /><br />
        Please login to the url below and add another note as a reply if required.<br /><br />
        The status and used hours can always be checked <br />
        <a href="<@serverUrl>/myportal/control/showPortletMainDecorator?portalPortletId=ViewCustRequest&amp;id=${custRequest.custRequestId}</@serverUrl>">here.....</a>
        <br /><br />
        Regards.<br /><br />
        Thank you for your business.
    </body>
</html>
