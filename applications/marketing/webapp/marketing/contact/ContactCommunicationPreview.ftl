<#--
This file is subject to the terms and conditions defined in
 file 'LICENSE', which is part of this source code package.
-->

<html>
    <head>
        <title>PREVIEW</title>
    </head>
    <body>
        <pre>
            From: ${(contactMech.infoString)!}
            Subject: ${(communicationEvent.subject)!}
        </pre>
        <hr />
        ${rawString(communicationEvent.content)!}

    </body>
</html>
