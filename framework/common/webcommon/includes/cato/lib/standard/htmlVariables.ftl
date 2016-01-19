<#--
* 
* Master HTML templating variables include, standard Cato markup.
*
* A set of global variables that define common styling classes, part of standard Cato Freemarker API.
* Automatically included at all times, unless overridden by properties or themes.
* Intended to be swappable.
*
* NOTE: currently targeted toward Foundation CSS.
*
* NOTE: variables must now be defined using #assign, not #global (treat this file as if it were being
*   included in its own namespace, even if it is not).
* 
-->

<#-- 
*************************************
* MAIN STYLES *
*************************************
* Cato automatically recognizes the "styles" global hash.
-->

<#assign styles = {
  <#-- Misc -->
    "framework" : "foundation",
    "disabled" : "disabled",
    "active" : "active",
    "button" : "button",
    "button_prefix" : "",
    "button_group" : "button-group",
    "button_force" : "force-button",
    "button_default" : "button tiny",
    "tiny" : "tiny",
    "small" : "small",
    "medium" : "medium",
    "large" : "large",
    "round" : "round",
    "radius" : "radius",
    "expand" : "expand",
    "collapse" : "collapse",
    "collapsed" : "collapsed",
    "required" : "required",
    "prefix" : "prefix",
    "postfix" : "postfix",
    "hidden" : "hide",
    "icon" : "fi",
    "icon_prefix" : "fi-",
    "icon_button" : "fi-widget",
    "icon_button_value" : "&#xf214;",
    "icon_calendar" : "fi-calendar",
    "icon_arrow" : "fi-arrow-right",
    "icon_list" : "fi-list",
    "icon_user" : "fi-torso-female",
    "icon_password" : "fi-lock",
    "icon_tenant" : "fi-cloud",
    "icon_error" : "fi-alert",
    "heading_level_prefix" : "heading-level-",  
    "switch" : "switch",
    "dropdown" : "dropdown",
    
  <#-- Menus 
       menu_xxx classes are looked up by the @menu and @menuitem macros as defaults for each menu type. 
       NOTE: currently the disabled/active/selected styles are added in addition to the base styles, 
          not replacing them; this is contrary to the current menu widgets implementation at the time of this
          writing, where disabled/active/selected styles replace their base styles.
          FIXME: we should use same behavior everywhere, but not clear which is more desirable at this time
              (menu widgets' way of replacing is more flexible, but not necessarily intuitive...)
          WARN: as of ofbiz 14.12 branch revision 1720933 the behavior is no longer clear or consistent for menu widgets
              (though it was before)
       --> 
    <#-- Generic menu types -->
    "menu_generic" : "",  
    "menu_generic_item" : "", 
    "menu_generic_item_link" : "",
    "menu_generic_item_submit" : "", 
    "menu_button" : "button-group force-button",
    "menu_button_item" : "",
    "menu_button_item_link" : "button tiny",
    "menu_button_item_submit" : "button tiny",
    <#-- Main navigation menus -->
    "menu_main" : "dropdown",
    "menu_main_wrap" : "has-dropdown not-click active",        
    "menu_main_item" : "",
    "menu_main_item_link" : "",
    "menu_main_item_submit" : "", 
    "menu_sidebar" : "side-nav",
    "menu_sidebar_wrap" :"", 
    "menu_sidebar_item" : "",
    "menu_sidebar_item_link" : "",
    "menu_sidebar_item_submit" : "", 
    <#-- Tab and secondary navigation menus-->
    "menu_tab" : "button-group force-button", 
    "menu_tab_item" : "", 
    "menu_tab_item_link" : "button tiny",
    "menu_tab_item_submit" : "button", 
    "menu_subtab" : "button-group force-button",
    "menu_subtab_item" : "",
    "menu_subtab_item_link" : "button tiny",
    "menu_subtab_item_submit" : "button tiny", 
    <#-- Default section menu -->
    "menu_section" : "button-group",
    "menu_section_item" : "",
    "menu_section_item_link" : "button tiny",
    "menu_section_item_submit" : "button tiny", 
    <#-- Default section menu inlined with title
        FIXME: looks too big/clumsy with these buttons (floats right of title) -->
    "menu_section_inline" : "button-group",
    "menu_section_inline_item" : "",
    "menu_section_inline_item_link" : "button tiny",
    "menu_section_inline_item_submit" : "button tiny", 
    <#-- default entry is used for any encountered menu types that have no specific entries in this hash, 
         and also for individual value fallbacks for values not defined in the type-specific entries above -->    
    "menu_default" : "",  
    "menu_default_item" : "", 
    "menu_default_itemdisabled" : "disabled", 
    "menu_default_itemactive" : "active", 
    "menu_default_itemselected" : "selected", 
    "menu_default_item_contentdisabled" : "disabled", 
    "menu_default_item_contentactive" : "active", 
    "menu_default_item_contentselected" : "selected", 
    "menu_default_item_link" : "",
    "menu_default_item_text" : "text-entry",
    "menu_default_item_submit" : "",  
    
    <#-- Misc menu styles -->
    "menu_subappitem" : "subappitem",
    "menu_subappitem_link" : "subapplink",
    
  <#-- Data and Tables -->  
    <#-- 
      NOTES:
        * currently, despite settings here, responsive tables will automatically be disabled for tables that have no header. see @table.
    -->
  
    <#-- table type-based styles 
         used for default table class and attribute lookups by the @table macro, based on table type. -->
    "table_generic" : "",    <#-- represents free-form <table>. probably should have no class (let template decide). -->
    "table_generic_cellspacing" : "",
    "table_generic_rowtype" : "generic",    <#-- the default row type is normally "content", but generic tables need "generic" -->
    "table_data_list" : "basic-table",  <#-- analogous to ofbiz "list" type form widgets (and still very close to "multi" type) -->
    "table_data_list_cellspacing" : 0,
    "table_data_list_responsive" : true,
    <#-- "table_data_list_autoaltrows" : true, -->
    "table_data_list_multiform" : "basic-table",  <#-- analogous to ofbiz "multi" type form widgets (but still basically a "list" type) -->
    "table_data_list_multiform_cellspacing" : 0,
    "table_data_list_multiform_responsive" : false,    
    "table_data_complex" : "basic-table", <#-- like data_list but with complex structure (no form widget equivalent) -->
    "table_data_complex_cellspacing" : 0,
    "table_summary" : "basic-table",
    "table_summary_cellspacing" : 0,
    "table_fields" : "basic-table",    <#-- similar to ofbiz "single" type form widgets; may be acceptable for display-only fields -->
    "table_fields_cellspacing" : 0,
    <#-- default entry is used by @table for any encountered table types that have no specific entries in this hash. 
         in other words, it is the default style for table types that this theme does not recognize, or "all others". 
         it is also used for individual fallback values. -->    
    "table_default" : "basic-table",
    "table_default_cellspacing" : "",
    "table_default_autoaltrows" : false,
    "table_default_rowtype" : "content",
    "table_default_responsive" : "",  <#-- boolean with support for empty value (ternary) -->
    "table_default_responsive_wrap" : "table-responsive", <#-- responsive table wrapping div element (TODO?) -->    
    "table_default_scrollable" : "", <#-- boolean with support for empty value (ternary) -->
    "table_default_responsive_options" : {
        "fixedHeader" : true,
        "scrollX" : true,
        "info" : false,
        "paging" : false,
        "searching" : false,
        "ordering" : true
    },
    "table_default_scrollable_options" : { <#-- this is only used for tables not marked to use responsive but need scrollable=true -->
        "fixedHeader" : false,
        "scrollX" : true,   <#-- note: this option gets overridden -->
        "info" : false,
        "paging" : false,
        "searching" : false,
        "ordering" : false
    },
    
    <#-- other table-related styles -->
    "table_basic" : "basic-table",  <#-- this is not a table type, but simply the basic-table style -->

    "row_reg" : "even",
    "row_alt" : "odd",
    "row_selected" : "selected",
    
  <#-- Generic action styles - help the UI describe actions possible in the system
      action_xxx styles are fully generic and meant to apply to any interactive UI element (link, menu item, onclick, etc.) and
      for any element that directly or indirectly triggers or encourages an action (nav link, submit button, etc.).-->
    <#-- Generic UI functionality action types -->
    "action_nav" : "action-nav",                          <#-- basically, identifies a navigation link. usually optional; added for clarity. -->
    "action_run" : "action-run",                          <#-- identifies a link or item that actually runs an action, such as a form submit button, or download PDF button, or intermediate action like clearing a form. nav links should never have this. -->
    
    <#-- Action state scope styles 
        By default, most actions are assumed to be action_scope_sys (database updates). 
        NOTE: these are decoupled from action_run, such that you could have these on a nav link (which 
            would mean, "this function will indirectly modify this scope once it's actually run"), though not really useful. -->
    "action_scope_sys" : "action-scope-sys",              <#-- action that changes system state, usually database -->
    "action_scope_session" : "action-scope-session",      <#-- action that changes user session state -->
    "action_scope_local" : "action-scope-local",          <#-- action that changes state on a page or form only (usually with javascript or input reset button) -->
    
    <#-- Convenience combinations of generic functionality + scope, useful on menu items, image links, and any non-text-link elements that identify or perform actions -->
    "action_nav_sys" : "action-nav action-scope-sys",
    "action_nav_session" : "action-nav action-scope-session",
    "action_nav_local" : "action-nav action-scope-local",
    "action_run_sys" : "action-run action-scope-sys",
    "action_run_session" : "action-run action-scope-session",
    "action_run_local" : "action-run action-scope-local",
    
    <#-- Specific action types 
        DEV NOTE: these are intended to support specific actions, but only specific actions that are reusable/applicable to many situations, such that while specific they should be able to apply to a number of different elements (from entity values to something farfetched like media playback) -->
    "action_generic" : "action-generic",                  <#-- generic action class; avoid using; always use more specific where possible -->
    "action_cancel" : "action-cancel",                    <#-- cancel (another) action class. WARN: this is only for cancelling other actions or navigations. it does not mean "set order status to cancelled" (use an action_modify for that, like action_terminate); is only for actions that cancel other actions in progress, not new requests for modifying system state. -->

    <#-- Specific state-changing actions -->
    "action_modify" : "action-modify",                    <#-- generic modify action, can also be a collection of modifications -->
    "action_add" : "action-modify action-add",            <#-- add item action: "Create Entity", "New Value", "Add", etc. -->
    "action_update" : "action-modify action-update",      <#-- update item action: "Update Entity", "Edit", etc. NOTE: this tends to be used for any action that updates, but it's better to use more specific ones -->
    "action_remove" : "action-modify action-remove",      <#-- (logical) remove item action: "Delete", "Remove", etc. -->
    "action_clear" : "action-modify action-clear",        <#-- clear action: "Clear", "Empty Fields", "Wipe", etc. -->
    "action_move" : "action-modify action-move",          <#-- move action: "Move", etc. -->
    "action_copy" : "action-modify action-copy",          <#-- copy action: "Copy", "Duplicate", etc. -->
    "action_configure" : "action-modify action-configure",<#-- configure action: "Configure", "Setup", etc. -->
    "action_begin" : "action-modify action-begin",        <#-- begin action: "Begin", "Start", "Start Job", etc. -->
    "action_reset" : "action-modify action-reset",        <#-- reset action: "Reset", "Restart", etc. -->
    "action_hold" : "action-modify action-hold",          <#-- hold action: "Hold", "Pause", etc. -->
    "action_terminate" : "action-modify action-terminate",<#-- terminate action: "Cancel Order", "Expire", "Stop", "Stop Job", etc. -->
    "action_complete" : "action-modify action-complete",  <#-- complete action: "Complete Order", "Mark Success", etc. -->
    "action_updatestatus" : "action-modify action-updatestatus",  <#-- update status action (any status not better accounted for in previous): "Approve", "Set to Xxx", etc. NOTE: this type was added later, so may be underused (action_update, action_terminate, action_complete usually found instead) -->
    "action_import" : "action-modify action-import",      <#-- upload action: "Import", "Upload", etc. -->
    "action_transfer" : "action-modify action-transfer",  <#-- generic transfer action: "Transfer", "Send", "Send Email", "Receive", etc. -->
    "action_send" : "action-modify action-send",          <#-- send action (transfer in send direction): "Send", "Send Email", etc. -->
    "action_receive" : "action-modify action-receive",    <#-- receive action (transfer in receive direction): "Receive", etc. -->
    "action_sync" : "action-modify action-sync",          <#-- sync action (two-way transfer): "Sync", etc. -->
    "action_register" : "action-modify action-login",     <#-- register action: "Register", etc.; usually goes with system scope -->
    "action_login" : "action-modify action-login",        <#-- login action: "Login", etc.; usually goes with session scope -->
    "action_logout" : "action-modify action-logout",      <#-- logout action: "Logout", etc.; usually goes with session scope -->

    <#-- Specific read-only actions (may or retrieve or submit data for analysis without changing state meaningfully) -->
    "action_readonly" : "action-readonly",                                <#-- readonly action or actions -->
    "action_read" : "action-readonly action-read",                        <#-- generic read action, can also be a collection of read actions -->
    "action_find" : "action-readonly action-read action-find",            <#-- find action: "Find", "Search", "Lookup", etc. -->
    "action_view" : "action-readonly action-read action-view",            <#-- view item action: "View PDF", etc. -->
    "action_export" : "action-readonly action-read action-export",        <#-- export action: "Export", "Download", "Stream", "Print", etc. -->
    
    "action_select" : "action-readonly action-select",                    <#-- select action: "Select", "Choose", "Pick", etc. -->
    "action_reload" : "action-readonly action-reload",                    <#-- reload action: "Refresh", etc. -->
    "action_verify" : "action-readonly action-verify",                    <#-- verify action (should not change system state!): "Verify", "Validate", etc. -->
    "action_close" : "action-readonly action-close",                      <#-- close action (should not change system state!): "Close Popup", etc. -->
    
    <#-- Misc action flags (fragments) -->
    "action_external" : "action-external",                <#-- external action, notably for marking external links -->
    
  <#-- Link styles (fragments) -->
    <#-- misc/short link styles -->
    "link_long" : "link-long",                            <#-- style to identify long links -->
    
    <#-- classes to identify content type of links 
        DEV NOTE (2016-01-19): these are not currently used enough to be useful, but they add clarification.
            they _should_ be used because it's difficult to impossible to select links based on child content in CSS without using javascript. -->
    "link_type_text" : "link-type-text",  
    "link_type_image" : "link-type-image",
    
  <#-- Link styles (standalone/full)
    DEV NOTE: 2016-01-14: the old use of link_nav and link_action made no real sense and has been ratified below.
      the old link_action has now been removed and turned mostly into link_nav(+action_xxx) and link_run_sys(+action_xxx).
  
    how to decide which style to use on a link:
      * the main classes of links are nav and run-action links. everything else is a specialization of these.
        * nav can be (and is) classified as a "navigation action" but called nav for short and for clarity.
      * IF THE LINK CONTAINS AN IMAGE INSTEAD OF TEXT, do not use the link_nav or link_run_xxx classes.
        instead, use action_nav and action_run_xxx. otherwise the CSS may assume wrongly that it's a link containing text and add a button.
        * e.g., <a href="remove" class="${styles.link_type_image!} ${styles.action_run_sys!} ${styles.action_remove!}" ><img .../></a>
        * DEV NOTE: having link_nav_image was not useful and confusing because most of the images were run-action links and few were nav links.
      * if the link's text designates a record (usually entity value) by name or ID alone or some combination of these, and basically "points to" a record,
        usually it should have link_nav_record_xxx. see "record identifiers and sorting fields" below.
        e.g. a link with an order ID (<a...>WS10000</a>) as its text that points to an order summary.
        * this is a special form/case of nav link, emphasized for styling needs.
      * if it's another basic navigation link or a link that leads to another page that leads to or encourages an action, see "navigation link".
        e.g., a "New" button that leads to a new page with a form where you can create a new entity value.
      * if it's a link that directly performs an action, see "run actions".
        e.g., the submit button ("Submit" or "Create New") on a form that trigger a new entity value creation.
      * a few rarer types have their own link_xxx styles such as link_nav_uri further below.
        * link_nav_uri would be a link with a URL as its text.
        * TODO: clarify how these fall in with the others
   
    navigation link (class="${styles.link_nav!} ${styles.action_add!}"):
      * these perform basic navigation, including navigation to pages from which run-actions are triggered and opening pop-up windows.
      * any basic navigation link can have link_nav (though note, link_nav like the other link_xxx may contain a core style like button style). 
      * if the navigation link leads to a page intended to perform a specific action, the link should be given link_nav
        along with an appropriate specific action_xxx action type style (see above), such as action_add, action_view, etc.
        * this is similar to some stock Ofbiz classes, but more generalized and better and supersedes the Ofbiz classes. 
          in original ofbiz you see such classes as link-style="create". they should be replaced by link-style="${styles.action_add}" or appropriate.
          the action_xxx classes can be applied to any interactive element, not only <a> links.
        * in principle, you could add a third action_scope_xxx style to "predict" the action scope, but is not very useful for nav links.
      * back pages and cancel buttons, even if they seem to be part of a form, as long as they behave as navigation links,
        should have the link_nav_cancel style. they need no extra styles.
      * action_view is more or less the default on link_nav and may be omitted.
      * for links containing images instead of text, use action_nav instead of link_nav
        e.g.: class="${styles.link_type_image!} ${styles.action_nav!} ${styles.action_add!}"
      
    run-actions (class="${styles.link_run_sys!} ${styles.action_add!}"):
      * these trigger actual actions in either system, session or local (page) scopes.
      * any link that performs an action that contacts the system to perform a new action should be given link_run_sys. this means
        form submits, PDF download, etc. link_run_sys contains the "action_run" class.
        they should also be given a second, specific action_xxx action type style (see above), such as action_add, 
        action_download, etc. or the more generic action_modify or action_readonly (or action_read) if not available.
        * by default, link_run_sys with any modify action is assumed to change the state of the system (usually database).
          so it means the user is committing to something, and it's usually the last button in a chain of links, like a 
          submit button (whereas link_nav is used to open the page for the action).
          * if the link is local to the page (page scope) and/or simply prepares for a submit or other action, it should be given
            link_run_local. this is mostly for javascript and input reset buttons. such links shouldn't
            really perform actions on the system (except read-only actions). they prepare other incoming system actions.
          * if the link only changes the state of the user's session, it should given link_run_session instead.
            NOTE: "session" is usually HTTP web session, but it extends to sessions which may be persisted. it is "logical" session.
        * the most important purpose of link_run_sys is to identify which links will change the state of the system,
          so user knows which changes are permanent and which functions don't commit anything.
          in the end, any link with the styles action_run, action_modify and action_scope_sys (or not scope)
          will be assumed to change the system state, or with action_scope_session, the session state (rare).
      * if it's a link to cancel another action like an upload, use link_run_sys_cancel instead of link_run_sys.
        * link_run_sys_cancel should not be used for things like changing order statuses to cancelled.
        * the same applies for link_run_session_cancel and link_run_local_cancel, though these are very rare.
      * should be used for buttons that change what's shown on the page, with action_view ("Show Old", visibility, etc.).
      * for links containing images instead of text, use action_run_xxx instead of link_run_xxx
        * e.g.: class="${styles.link_type_image!} ${styles.action_run_sys!} ${styles.action_add!}"

    record identifiers and sorting fields (class="${styles.link_nav_record_id!}"):
      * these are essentially specific versions of link_nav for styling purposes; they identify the text the links have as labels.
        * such text can occasionally be found on non-nav (action) links, but this is rare, and to simplify some kinds of styling
          these are integrated under link_nav_xxx.
      * if the text is a simple single record (entity) id, name, date, or other record identifier, use link_nav_record_id, link_nav_record_name, link_nav_record_date,
        or if the text type is not listed or cannot be known in advance, link_nav_record_value.
        * if the value is complex or very long (e.g. multiple-field primary key), or possibly if has introductory words ("Order Item: 1000000"),
          use the corresponding link_longxxx variant instead.
      * if it's a combination of name and id, use link_nav_record_idname (or link_nav_record_idname_long). however, if it's
        a name with a default value fallback to id, use link_nav_record_name (or link_nav_record_name_long).
      * if it's a more complex combination or a description (other than id + name), use link_nav_record_desc.
      * simple extraneous characters like brackets around an id should not affect selection;
        but introductory words ("Order Item: WS10000") may warrant putting it under the link_longxxx variant.
        in some cases, extra words means it should go under link_nav_record_desc.
      * it can sometimes optionally be given a second action_xxx style if it encourages an action, such as action_update, action_add, etc.
        action_view is the implied default, can be set explicitly.
      * these styles are for navigation links. if they directly perform actions, probably use run actions instead (see "run actions", e.g. link_run_sys).
      
    other notes:  
      * every link style has a _long version for entries with long labels.
      * the cancel links are exceptions: they all have separate entries because often you might want a completely different button on them
        (which you can do with CSS but not as easily with link_run_sys+action_cancel). they are not regular actions in and of themselves - they are anti-actions.
      * action_view actions are often ambiguous and could be either link_nav or link_run_sys depending on UI functionality and perspective.
        * as a general rule, link_run_sys are for the "final" actions (like submit buttons), while link_nav are for links heading toward the action (or that open a page).
          however, for view action, sometimes the line is not clear and subjective or there is no distinction. should be judged based on context.
      * external links and actions should be marked with ${styles.action_external!}    
          
    DEV NOTES:
      * regarding link_nav_record_xxx and similar: these could have had a second layer of categories such as link_record_xxx
        which aren't bound to imply link_nav, so they could be reused on link_run_xxx, but it's so rare there's probably no real need (link_run_xxx with _long variants is enough for these rare cases).
        the original link_record_xxx classes were renamed to link_nav_record_xxx because they practically all already implied link_nav everywhere, just wasn't clear enough.
        this link_nav_record_xxx pattern is not consistent with the action_xxx pattern (above), which is not great, but this could be justified by styling needs.
        if find there is a new need, can always create link_record_xxx add-on classes similar to action_xxx which don't imply link_nav.
        
    TODO:
      * move documentation and compactify what's left here
  -->
    <#-- Action text links (trigger an actual action in the system - NOT for use for opening pages toward actions!) -->
    "link_run_sys" : "link-type-text button tiny action-run action-scope-sys",                                  <#-- link that actually performs an action (run-action), in system scope, such as most form submit buttons, "Create Order", "Download PDF", etc. -->
    "link_run_sys_long" : "link-type-text action-run action-scope-sys link-long",
    "link_run_sys_cancel" : "link-type-text button tiny action-run action-scope-sys action-cancel",             <#-- link that cancels a system action in progress, such as cancelling an upload (but NOT if only a button that leads back to previous page - use link_nav_action_cancel, and NOT for changing an order status to cancelled - use link_run_sys with appropriate action_xxx appended) -->
    "link_run_sys_cancel_long" : "link-type-text action-run action-scope-sys action-cancel link-long",
    "link_run_session" : "link-type-text button tiny action-run action-scope-session",                          <#-- link for any action (run-action) that only modifies current session (logical, not necessarily HTTP session), not meaningful permanent system data. -->
    "link_run_session_long" : "link-type-text action-run action-scope-session link-long",
    "link_run_session_cancel" : "link-type-text button tiny action-run action-scope-session action-cancel",     <#-- link for any action that cancels another session action (rare). -->
    "link_run_session_cancel_long" : "link-type-text action-run action-scope-session action-cancel link-long",
    "link_run_local" : "link-type-text button tiny action-run action-scope-local",                              <#-- link for any action (run-action) local to a page or that prepares a page for another action, such as "Clear" or "Reset" buttons that empty a form or form field and interactive javascript forms. -->
    "link_run_local_long" : "link-type-text action-run action-scope-local link-long",
    "link_run_local_cancel" : "link-type-text button tiny action-run action-scope-local action-cancel",         <#-- link for any action that cancels another page-scope action (rare). -->
    "link_run_local_cancel_long" : "link-type-text action-run action-scope-local action-cancel link-long",

    <#-- General navigation text links (basic navigation and navigation toward actions; note that navigation itself is considered an action)
        NOTE: unlike link_run_xxx, we omit sys/session/local scope from these because "predicting" the scope of an action in a nav link is not really useful in a UI. -->
    "link_nav" : "link-type-text button tiny action-nav",                             <#-- navigation link toward another page, usually with static text like "New" or "Edit" or "View". the link should also be qualified with an "action_xxx" class where appropriate (see above), to indicate the action that the link is leading the user to do. -->
    "link_nav_long" : "link-type-text action-nav link-long",                          <#-- very long or complex/non-static nav/viewing link: "Categories: All Products Sorted by Name" -->
    "link_nav_cancel" : "link-type-text button tiny action-nav action-cancel",        <#-- back/cancel/done navigation link that leads back to another page (could be said as: "cancels" the nagivation action): "Back", "Cancel", "Done", etc. NOTE: may often appear as if is part of a form submit (run action), but is not really. -->
    "link_nav_cancel_long" : "link-type-text action-nav action-cancel link-long",

    <#-- Navigation text links with specific label text 
        DEV NOTE (2016-01-19): these have become less important with the new categorizations (the categories above are more important to be followed for good user UI) and because all the categories above support _long versions. however I see no real harm in leaving these in for now (except for consistency concerns)... adds extra configure options. if found to be not needed, they could simply be all changed back to simple link_nav[_long].
            there is also an issue where "button tiny" is not often wanted within form widget tables apparently (was removed from these)... changing them back to link_nav would readd it...
            TOOD? may need CSS to identify to style differently depending on if these land within form tables vs outside. but should probably do that with CSS selectors instead of here...
                maybe remove "button tiny" from all link_nav and link_run_xxx and delegate to SCSS.
        FIXME: a few of these that were used in templates earlier may actually belong as link_run_xxx instead of link_nav... should review templates again... -->
    "link_nav_record_id" : "link-type-text action-nav",                         <#-- the short ID or unique code of a record (1-20 chars): "WS10000", "10000", "ORDER_CANCELLED", etc. -->
    "link_nav_record_id_long" : "link-type-text action-nav link-long",          <#-- the long ID of a record (more than 20-30 chars), records that do not have single IDs, and IDs with long extraneous words: "WS10000-ITEM10000", "Workspace-Timesheet: TS100000" -->
    "link_nav_record_name" : "link-type-text action-nav",                       <#-- the name of a record: "My Order 23", "Some Value", "Cancelled", etc. -->
    "link_nav_record_name_long" : "link-type-text action-nav link-long",        <#-- the long name of a record: "Mr. Title The Ambassador of Germany", etc. -->
    "link_nav_record_idname" : "link-type-text action-nav",                     <#-- the name and id of a record: "My Order 23 (WS10000)", "WS10000 (My Order 23)" etc. -->
    "link_nav_record_idname_long" : "link-type-text action-nav link-long",      <#-- long combination of IDs and names: "Mr. John Alberton Smith Junior (ID: 10000) (Group: 20000)" -->
    "link_nav_record_desc" : "link-type-text action-nav",                       <#-- the description of a record: "Order that was placed by admin", "This is some value", "This means order cancelled", etc. In general, as soon as a link text contains more than one type of value, and not idname, it should be changed to link_nav_record_desc. -->
    "link_nav_record_date" : "link-type-text action-nav",                       <#-- the date of a record (fromDate, thruDate, etc.) -->
    "link_nav_record_number" : "link-type-text action-nav",                     <#-- the number of a record (index, sequence num, etc.) -->
    "link_nav_record_value" : "link-type-text action-nav",                      <#-- link containing a value of type not previously listed (or cannot be known statically) -->
    "link_nav_record_value_long" : "link-type-text action-nav link-long",       <#-- link containing a value of type not previously listed but that may be long (or cannot be known statically) -->
    "link_nav_uri" : "link-type-text action-nav",                               <#-- link containing a URL, path or other location as its text (<a href="http://ofbiz.apache.org">http://ofbiz.apache.org</a>); may be IP, hostname, etc. -->
    "link_nav_text" : "link-type-text action-nav",                              <#-- link containing any kind of free-form text -->
    
  <#-- Colors -->
    "color_green" : "success",
    "color_red" : "alert",
    "color_grey" : "secondary",
    "color_primary" : "primary",
    "color_secondary" : "secondary",
    "color_info" : "info",
    "color_warning" : "warning",
    "color_success" : "success",  
    "color_alert" : "alert",        <#-- Bootstrap: "danger" -->
    "color_error" : "alert",
    
    "button_color_default" : "",
    "button_color_primary" : "primary",
    "button_color_secondary" : "secondary",
    "button_color_success" : "success",
    "button_color_info" : "info",
    "button_color_warning" : "warning",
    "button_color_alert" : "alert",
    
  <#-- Grid -->
    "grid_row" : "row",
    "grid_cell" : "columns",
    "grid_cell_default" : "large-12",
    "grid_display" : "display",      <#-- display grid -->
    "grid_end" : "end",
    "grid_small" : "small-",   
    "grid_medium" : "medium-",
    "grid_large" : "large-",       <#-- Bootstrap: "col-lg-" -->
    "grid_offset" : "offset-",      <#-- Bootstrap: "col-md-offset-" -->
    "grid_small_offset" : "small-offset-",     
    "grid_medium_offset" : "medium-offset-",    
    "grid_large_offset" : "large-offset-",   
    "grid_block_prefix" : "",
    "grid_block_postfix" : "block-grid-",
    "grid_columns_1" : "large-1 columns",
    "grid_columns_2" : "large-2 medium-3 columns",
    "grid_columns_3" : "large-3 medium-4 columns",
    "grid_columns_4" : "large-4 medium-5 columns",
    "grid_columns_5" : "large-5 medium-6 columns",
    "grid_columns_6" : "large-6 columns",
    "grid_columns_7" : "large-7 medium-6 columns",
    "grid_columns_8" : "large-8 medium-7 columns",
    "grid_columns_9" : "large-9 medium-8 columns",
    "grid_columns_10" : "large-10 medium-9 columns",
    "grid_columns_11" : "large-11 medium-10 columns",
    "grid_columns_12" : "large-12 columns",
    "grid_centered" : "centered",
    "grid_small_centered" : "small-centered",
    "grid_medium_centered" : "medium-centered",
    "grid_large_centered" : "large-centered",
    "grid_sidebar_0_main" : "large-12 columns",
    "grid_sidebar_1_main" : "large-10 medium-9 columns",
    "grid_sidebar_1_side" : "large-2 medium-3 columns",
    "grid_sidebar_2_main" : "large-8 medium-6 columns",
    "grid_sidebar_2_side" : "large-2 medium-3 columns",

  <#-- Text align -->
    "text_left" : "text-left",
    "text_right" : "text-right",
    "text_center" : "text-center",
    "text_justify" : "text-justify",

  <#-- Floats -->
    "float_left" : "left",
    "float_right" : "right",
    "float_clearfix" : "clearfix",
    
  <#-- Navigation -->
    "nav_subnav" : "sub-nav",
    "nav_sidenav" : "navbar-inverse collapse navbar-collapse navbar-ex1-collapse ",
    
    <#-- Breadcrumbs -->
    "nav_breadcrumbs" : "breadcrumbs",                  <#-- breadcrumb container -->
    "nav_breadcrumb" : "",                              <#-- breadcrumb entry -->
    <#-- NOTE: currently the active/disabled styles are added in addition to nav_breadcrumb, not replacing them -->
    "nav_breadcrumb_disabled" : "unavailable",
    "nav_breadcrumb_active" : "current",
    "nav_breadcrumb_link" : "",                         <#-- breadcrumb link -->
  
  <#-- Pagination -->
    "pagination_wrap" : "pagination-centered",
    "pagination_list" : "pagination",
    "pagination_control" : "",
    "pagination_item" : "",
    "pagination_item_first" : "nav-first",
    "pagination_item_last" : "nav-last",
    "pagination_item_previous" : "nav-previous",
    "pagination_item_next" : "nav-next",
    "pagination_item_active" : "current",
    "pagination_item_disabled" : "unavailable",
    "pagination_layout" : "bottom",
    "pagination_noresultsmode" : "hide",
    "pagination_showcount" : true,           <#-- show count by default -->
    "pagination_alwaysshowcount" : true,     <#-- show count even if no pagination controls -->
    "pagination_countmsglabel" : "CommonDisplayingShort",     <#-- default count message property; must be in CommonUiLabels.xml -->
    "pagination_lowcountmsglabel" : "CommonDisplayingShort",  <#-- default count message property for low counts; must be in CommonUiLabels.xml -->
    
  <#-- Lists -->
    "list_inline" : "inline-list",

  <#-- Tile -->
    <#-- tiles-type-based args and styles
         as always, default is also used for individual value fallbacks when missing from specific types. 
         NOTE: currently most of these values are macro args, which may not be straight stylenames (they get mapped to individual styles further below) 
            see @tile macro interface. it's perfectly possible to ignore them and delegate styling to css by addressing the right classes.
            for example, the *bgcolor can be set to the value "none" to prevent setting a color class.
         DEV NOTE: the bg colors may seem like overkill, but they avoid redundancy in the CSS maybe? whatever, more configurable this way -->
    "tile_default_containerclass" : "tile-container-default", 
    "tile_default_class" : "tile-default",  
    "tile_default_size" : "normal",
    "tile_default_color" : "0",
    "tile_default_titletype" : "default",
    "tile_default_titlebgcolor" : "default-title",
    "tile_default_overlaytype" : "default",   <#-- "default" will be mapped to "tile_overlay_default" style below -->
    "tile_default_overlaybgcolor" : "0",
    "tile_default_imagetype" : "default",
    "tile_default_imagebgcolor" : "none",
    "tile_default_icon" : "",
    "tile_default_linktarget" : "",
    
    "tile_generic_containerclass" : "tile-container-generic", 
    "tile_generic_class" : "tile-generic",  
    "tile_generic_size" : "normal",
    "tile_generic_color" : "none",
    "tile_generic_titletype" : "default",
    "tile_generic_titlebgcolor" : "none",
    "tile_generic_overlaytype" : "contain",
    "tile_generic_overlaybgcolor" : "none",
    "tile_generic_imagetype" : "cover",
    "tile_generic_imagebgcolor" : "none",
    "tile_generic_icon" : "",
    "tile_generic_linktarget" : "",
    
    "tile_gallery1_containerclass" : "tile-container-gallery1", 
    "tile_gallery1_class" : "tile-gallery1",  
    "tile_gallery1_size" : "normal",
    "tile_gallery1_color" : "0",
    "tile_gallery1_titletype" : "default",
    "tile_gallery1_titlebgcolor" : "gallery-title-bg",  <#-- (delegated to CSS) -->
    "tile_gallery1_overlaytype" : "default",
    "tile_gallery1_overlaybgcolor" : "gallery-overlay-bg",   <#-- (delegated to CSS) -->
    "tile_gallery1_imagetype" : "contain",
    "tile_gallery1_imagebgcolor" : "none",
    "tile_gallery1_icon" : "",
    "tile_gallery1_linktarget" : "_blank",

    <#-- low-level tile individual styles -->
    "tile_container" : "tile-container",
    "tile_wrap" : "tile",
    "tile_color_prefix" : "tile-color-",
    "tile_content" : "tile-content",
    "tile_icon" : "tile-icon",
    <#-- title styles -->
    "tile_title" : "tile-title",
    <#-- default title styles (looked up based on title type) -->
    "tile_title_default" : "tile-title-default",
    <#-- overlay styles -->
    "tile_overlay" : "tile-overlay",
    <#-- default overlay styles (looked up based on overlay type) -->
    "tile_overlay_default" : "tile-overlay-slide-up",    
    <#-- styles for specific overlay types (extensible by theme) (looked up based on overlay type) -->
    "tile_overlay_slide_up" : "tile-overlay-slide-up",
    <#-- image styles -->
    "tile_image" : "tile-image",
    <#-- default image styles (looked up based on image type) -->
    "tile_image_default" : "tile-image-cover",
    <#-- styles for specific tile types (extensible by theme) (looked up based on image type) -->
    "tile_image_cover" : "tile-image-cover",
    "tile_image_contain" : "tile-image-contain",
    
  <#-- Image galleries (non-tile settings - tile settings contained in tile styles) -->
    "gallery_share_view_width" : 500,
    "gallery_share_view_height" : 500,

  <#-- Panels -->
    "panel_wrap" : "panel",
    "panel_head" : "",
    "panel_title" : "",
    "panel_body" : "",
    "login_wrap" : "large-3 login-box",                  <#-- the login box is a special form of a panel in this case -->
    "login_header": "",
    "login_body" : "signup-panel right-solid",

  <#-- Alert Box -->
    "alert_wrap" : "alert-box",    <#-- Bootstrap: "alert" -->
    "alert_prefix_type" : "",             <#-- Bootstrap: "alert-"; Note: This may be removed -->
    "alert_type_info" : "info",
    "alert_type_success" : "success",
    "alert_type__warning" : "warning",
    "alert_type_secondary" : "secondary",  
    "alert_type_alert" : "alert",        
    "alert_type_error" : "alert",

  <#-- Pricing Tables -->
    "pricing_wrap" : "pricing-table",
    "pricing_price" : "price",
    "pricing_description" : "description",
    "pricing_title" : "title",
    "pricing_cta" : "cta-button",
    "pricing_bullet" : "bullet-item",

  <#-- Modal -->
    "modal_wrap" : "reveal-modal",

  <#-- Chart -->
    "chart_legend" : "chart-legend",

  <#-- Progress Bar -->
    "progress_container" : "progress",   <#-- Bootstrap: "progress" -->
    "progress_wrap" : "",           <#-- Bootstrap: "progress-bar" -->
    "progress_bar" : "meter",       <#-- Bootstrap: "sr-only" -->

  <#-- Fields 
       fields_xxx classes and settings are looked up by the @fields and @field macros as defaults for
       fields type and field type. -->
    <#-- fields type-based styles 
        NOTE: fields_default_xxx is also used as a fallback for the more specialized type for any
            missing individual definitions (same as menu and table defs) 
            the other ones simply override them. -->
    "fields_default_labeltype" : "horizontal",
    "fields_default_labelposition" : "left",
    "fields_default_labelarea" : true,
    "fields_default_labelareaexceptions" : "submit submitarea",
    "fields_default_labelarearequirecontent" : false,
    "fields_default_collapse" : false,
    "fields_default_collapsepostfix" : true,
    "fields_default_collapsedinlinelabel" : "datetime", <#-- can be true, false, or list of type names -->

    "fields_default_nolabels_labeltype" : "horizontal", <#-- note: this should be "none" in principle, but this is used to implement collapsedinlinelabel. labelarea false is good enough. -->
    "fields_default_nolabels_labelposition" : "left",
    "fields_default_nolabels_labelarea" : false,
    "fields_default_nolabels_labelareaexceptions" : "", 
    "fields_default_nolabels_labelarearequirecontent" : false,

    "fields_default_compact_labeltype" : "vertical",
    "fields_default_compact_labelposition" : "top",
    "fields_default_compact_labelarea" : true, 
    "fields_default_compact_labelareaexceptions" : "radio checkbox", <#-- having label on top of checkbox or radio is really ugly; let the label inline itself for these -->
    "fields_default_compact_labelarearequirecontent" : true,

    "fields_generic_labeltype" : "horizontal",
    "fields_generic_labelposition" : "left",
    "fields_generic_labelarea" : false,
    "fields_generic_labelareaexceptions" : "", 
    "fields_generic_labelarearequirecontent" : false, 
    "fields_generic_collapse" : false,
    "fields_generic_collapsepostfix" : true,
    "fields_generic_collapsedinlinelabel" : "", <#-- can be true, false, or list of type names -->


    <#-- misc field styles -->
    "fields_wrap" : "collapse",
    "fields_label" : "prefix",

    <#-- Field type style name maps
        These map cato and ofbiz field types to style names to represent the fields in CSS, so they can be unified.
        For any entries not specified, "default" entry is consulted.
        The boolean true means use the cato or ofbiz type name as style name.
        Note these names are not full CSS style names, but prefixes or suffixes. -->
    "field_type_stylenames_cato" : {
        "default": true
    },
    "field_type_stylenames_ofbiz" : {
        "display": "display",
        "hyperlink": "hyperlink",
        "text": "input",
        "textarea": "textarea",
        "date-time": "datetime",
        "drop-down": "select",
        "check": "checkbox",
        "radio": "radio",
        "submit": "submit",
        "reset": "reset",
        "hidden": "hidden",
        "ignored": "ignored",
        "text-find": "textfind",
        "date-find": "datefind",
        "range-find": "rangefind",
        "lookup": "lookup",
        "file": "file",
        "password": "password",
        "image": "image",
        "display-entity": "displayentity",
        "container": "container",
        "default": "other"
    },
    
    <#-- field types that should not get containers when they are children of other fields, by default -->
    "field_type_nocontainer_whenchild" : {
        <#-- "submit":true -->   <#-- only if parent is submitarea (below) -->
        "radio":true,
        "checkbox":true,
        "option":true
    },
    
    <#-- field types that when a child field has as parent, the child should not get a container, by default
        (in other words this parent's children should not get containers, by default) -->
    "field_type_nocontainer_whenhasparent" : {
        "submitarea":true
    },
    
    
    <#-- Form type to @table type maps -->
    "form_type_tabletypes_ofbiz" : {
        "list": "data-list",
        "multi": "data-list-multiform",
        "default": "generic"
    },
    
    <#-- Field select element multiple-select classes (jQuery asmselect) -->
    "field_select_asmselect" : {
      "containerClass" : "asmContainer",            <#-- Class for container that wraps this widget -->
      "selectClass" : "asmSelect",                  <#-- Class for the newly created <select> -->
      "optionDisabledClass" : "asmOptionDisabled",  <#-- Class for items that are already selected / disabled -->
      "listClass" : "asmList",                      <#-- Class for the list ($ol) -->
      "listSortableClass" : "asmListSortable",      <#-- Another class given to the list when it is sortable -->
      "listItemClass" : "asmListItem",              <#-- Class for the <li> list items -->
      "listItemLabelClass" : "asmListItemLabel",    <#-- Class for the label text that appears in list items -->
      "removeClass" : "asmListItemRemove button tiny",          <#-- Class given to the "remove" link -->
      "highlightClass" : "asmHighlight"             <#-- Class given to the highlight <span> -->
      <#-- "listType" : "ol",           Ordered list 'ol', or unordered list 'ul' -->
      <#-- "sortable" : false,          Should the list be sortable? -->
      <#-- "highlight" : false,         Use the highlight feature? -->
      <#-- "animate" : false,           Animate the the adding/removing of items in the list? -->
      <#-- "addItemTarget" : "bottom",        Where to place new selected items in list: top or bottom -->
      <#-- "hideWhenAdded" : false,         Hide the option when added to the list? works only in FF -->
      <#-- "debugMode" : false,         Debug mode keeps original select visible -->

      <#-- "removeLabel" : "remove",          Text used in the "remove" link -->
      <#-- "highlightAddedLabel" : "Added: ",       Text that precedes highlight of added item -->
      <#-- "highlightRemovedLabel" : "Removed: "     Text that precedes highlight of removed item -->
    },

  <#-- Always declare last -->
    "dummy" : ""
}>

<#-- 
*************************************
* OTHER TEMPLATING GLOBALS *
*************************************
-->

<#-- (currently none) -->


