/**
 * SCIPIO: for doc browsing
 */

if (parameters.docWidePage == 'true' || Boolean.TRUE.equals(parameters.docWidePage)) {
    context.widePage = true;
    session.setAttribute('docWidePageSaved', true);
} else if (parameters.docWidePage == 'false' || Boolean.FALSE.equals(parameters.docWidePage)) {
    context.widePage = false;
    session.setAttribute('docWidePageSaved', false);
} else {
    if (Boolean.TRUE.equals(session.getAttribute('docWidePageSaved'))) {
        context.widePage = true;
    } else {
        context.widePage = false;
    }
}
