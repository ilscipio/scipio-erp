<#--
This file is subject to the terms and conditions defined in the
files 'LICENSE' and 'NOTICE', which are part of this source
code package.
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
        ${raw(communicationEvent.content)!}

    </body>
</html>
