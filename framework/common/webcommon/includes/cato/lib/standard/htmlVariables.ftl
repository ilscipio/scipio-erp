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
    
  <#-- Generic action styles, describe actions possible in the system in UI
      action_xxx styles are fully generic and meant to apply to any interactive UI element (link, menu item, onclick, etc.) and
      for any element that directly or indirectly triggers or encourages an action (nav link, submit button, etc.).-->
    
  <#-- General UI functionality action types -->
    "action_nav" : "action-nav",                          <#-- basically, identifies a navigation link. usually optional; added for clarity. -->
    "action_run" : "action-run",                          <#-- identifies a link or item that actually runs an action, such as a form submit button, or download PDF button, or intermediate action like clearing a form. nav links should never have this. -->
    
  <#-- Action state scope styles 
      By default, most actions are assumed to be action_scope_system (database updates). 
      NOTE: these are decoupled from action_run, such that you could have these on a nav link (which 
          would mean, "this function will indirectly modify this scope once it's actually run"), though not really useful. -->
    "action_scope_system" : "action-scope-system",        <#-- action that changes system state, usually database -->
    "action_scope_session" : "action-scope-session",      <#-- action that changes user session state -->
    "action_scope_page" : "action-scope-page",            <#-- action that changes state on a page or form only (usually with javascript or input reset button) -->
    
  <#-- Specific action types -->
    "action_generic" : "action-generic",                  <#-- generic action class; avoid using; always use more specific where possible -->
    "action_cancel" : "action-cancel",                    <#-- cancel (another) action class. WARN: this is only for cancelling other actions or navigations. it does not mean "set order status to cancelled" (use an action_modify for that, like action_terminate); 
                                                              is only for actions that cancel other actions in progress, not new requests for modifying system state. -->
    <#-- state-changing actions -->
    "action_modify" : "action-modify",                    <#-- generic modify action, can also be a collection of modifications -->
    "action_create" : "action-modify action-create",      <#-- create item action link: "Create Entity", "New Value", "Add", etc. -->
    "action_update" : "action-modify action-update",      <#-- update item action link: "Update Entity", "Edit", etc. -->
    "action_remove" : "action-modify action-remove",      <#-- (logical) remove item action link: "Delete", "Remove", etc. -->
    "action_clear" : "action-modify action-clear",        <#-- clear action link: "Clear", "Reset", "Empty Fields", etc. -->
    "action_copy" : "action-modify action-copy",          <#-- copy action link: "Copy", "Duplicate", etc. -->
    "action_configure" : "action-modify action-configure",<#-- configure action link: "Configure", "Setup", etc. -->
    "action_begin" : "action-modify action-begin",        <#-- begin action link: "Begin", "Start", "Start Job", etc. -->
    "action_terminate" : "action-modify action-terminate",<#-- terminate action link: "Cancel Order", "Expire", "Stop", "Stop Job", etc. -->
    "action_complete" : "action-modify action-complete",  <#-- complete action link: "Complete Order", "Mark Success", etc. -->
    "action_import" : "action-modify action-import",      <#-- upload action link: "Import", "Upload", etc. -->
    "action_transfer" : "action-modify action-transfer",  <#-- transfer action link: "Transfer", "Send", "Send Email", etc. -->

    <#-- read-only actions -->
    "action_read" : "action-read",                        <#-- generic read action, can also be a collection of read actions -->
    "action_find" : "action-read action-find",            <#-- find action link: "Find", "Search", "Lookup", etc. -->
    "action_select" : "action-read action-select",        <#-- select action link: "Select", "Choose", "Pick", etc. -->
    "action_view" : "action-read action-view",            <#-- view item action link: "View PDF", etc. -->
    "action_export" : "action-read action-export",        <#-- export action link: "Export", "Download", "Stream", etc. -->

  <#-- Standalone link styles (includes links in tables)
    DEV NOTE: 2016-01-07: the old use of link_nav and link_action made no real sense and has been ratified below.
      link_action will be removed and turned mostly into link_nav(+action_xxx) and link_action_run(+action_xxx).
  
    how to decide which style to use on a link:
      * if the link designates a record (usually entity value) by name or ID alone or some combination, basically "points to" a record,
        usually it should have link_record_xxx. see "record identifiers and sorting fields" below.
        * it's a special form of navigation link, made different for styling reasons.
      * if it's a basic navigation link or a link that leads to another page that leads to or encourages an action, see "navigation link".
      * if it's a link that directly performs an action such as a submit button, see "run actions".
      * simpler links such as external links have their own link_xxx styles such as link_url.
   
    navigation link (class="${styles.link_nav!} ${styles.action_create!}"):
      * any basic navigation link can have link_nav (though note, link_nav like the other link_xxx may contain a core style like button style). 
      * if the navigation link leads to a page intended to perform a specific action, the link should be given link_nav
        along with an appropriate specific action_xxx action type style (see above), such as action_create, action_view, etc.
        this is similar to stock Ofbiz use of classes on menu items and helpful visually.
        * in principle, you could add a third action_scope_xxx style to "predict" the action, but is not useful.
      * back pages and cancel buttons, even if they seem to be part of a form, as long as they behave as navigation links,
        should have the link_nav_cancel style. they need nothing else.
      
    run actions (class="${styles.link_action_run!} ${styles.action_create!}"):
      * any link that performs an action that contacts the system to perform a new action should be given link_action_run. this means
        form submits, PDF download, etc. link_action_run contains the "action_run" class.
        they should also be given a specific action_xxx action type style (see above), such as action_create, 
        action_download, etc. or the more generic action_modify or action_read if not available.
        * by default, link_action_run with any modify action is assumed to change the state of the system (usually database).
          if it only changes session, it can optionally be given a scope style such as action_scope_session (rare).
        * the most important purpose of link_action_run is to identify which links will change the state of the system.
          in the end, any link with the styles action_run, action_modify and action_scope_system (or not scope)
          will be assumed to change the system state, or with action_scope_session, the session state (rare).
      * if it's a link to cancel another action like an upload, use link_action_run_cancel instead of link_action_run.
        * link_action_run_cancel should not be used for things like changing order statuses to cancelled.
      * if the link is local to the page and/or simply prepares for a submit or other action, it should be given
        link_action_prepare. this is mostly for javascript and input reset buttons. such links shouldn't
        really perform actions on the system. they prepare other actions.
      * rarely, a cancel button to cancel a preparation action could happen, in which case link_action_prepare_cancel should be used.

    record identifiers and sorting fields (class="${styles.link_record_id!}"):
      * these are essentially specific versions of link_nav.
      * if the text is a simple single record (entity) id, name, date, or other record identifier, use link_record_id, link_record_name, link_record_date,
        or if the text type is not listed or cannot be known in advance, link_record_value.
        * if the value is complex or very long (e.g. multiple-field primary key), or possibly if has introductory words ("Order Item: 1000000"),
          use the corresponding link_longxxx variant instead.
      * if it's a combination of name and id, use link_record_idname (or link_record_idname_long). however, if it's
        a name with a default value fallback to id, use link_record_name (or link_record_name_long).
      * if it's a more complex combination or a description (other than id + name), use link_record_desc.
      * simple extraneous characters like brackets around an id should not affect selection;
        but introductory words ("Order Item: WS10000") may warrant putting it under the link_longxxx variant.
        in some cases, extra words means it should go under link_record_desc.
      * it can sometimes optionally be given a second action_xxx style if it encourages an action, such as action_view, action_create, etc.
      * these styles are for navigation links. if they directly perform actions, probably use run actions instead (see "run actions", e.g. link_action_run).
      
    other notes:  
      * every link style has a _long version for entries with long labels.
      * the cancel links are exceptions: they all have separate entries because often you might want a completely different button on them
        (which you can't do with link_action_run+action_cancel). they are not regular actions in and of themselves - they are anti-actions.
  -->
    "link_nav" : "button tiny action-nav",                              <#-- navigation link toward another page, usually with static text like "New" or "Edit" or "View".
                                                                            the link should also be qualified with an "action_xxx" class where appropriate (see above), to indicate
                                                                            the action that the link is leading the user to do. -->
    "link_nav_long" : "action-nav link-long",                           <#-- very long or complex/non-static nav/viewing link: "Categories: All Products Sorted by Name" -->
    "link_nav_cancel" : "button tiny action-nav action-cancel",         <#-- back/cancel navigation link that leads back to another page: "Back", "Cancel", etc. NOTE: can often appear as if is part of a form submit (run action), but is not really. -->
    "link_nav_cancel_long" : "action-nav action-cancel link-long",

    "link_action_run" : "button tiny action-run",                       <#-- link that actually performs an action, such as most form submit buttons, "Create Order", "Download PDF", etc. -->
    "link_action_run_long" : "action-run link-long",
    "link_action_run_cancel" : "button tiny action-run action-cancel",  <#-- link that cancels an action in progress, such as cancelling an upload (but NOT if only a button that leads back to previous page - use link_nav_action_cancel, 
                                                                            and NOT for changing an order status to cancelled - use link_action_run with appropriate action_xxx appended) -->
    "link_action_run_cancel_long" : "action-run action-cancel link-long",
    "link_action_prepare" : "button tiny action-run action-scope-page", <#-- link for any action that prepares a page for another action, such as "Clear" or "Reset" buttons that empty a form or form field. can also be used for heavily interactive javascript forms. -->
    "link_action_prepare_long" : "action-run action-scope-page link-long",
    "link_action_prepare_cancel" : "button tiny action-run action-scope-page action-cancel", <#-- link for any action that cancels another preparation action (rare). -->
    "link_action_prepare_cancel_long" : "action-run action-scope-page action-cancel link-long",

    "link_action" : "button tiny action-generic",               <#-- DEPRECATED; TO BE REMOVED [use link_nav and link_action_run instead]: action link: "Add", "Edit", "Remove", "Cancel", "Export as PDF", "Edit: WS10000", etc. not necessarily a verb, action may be implied, but should be an action. usually static text. 
                                                                    this may overlap with link_nav, but usually there is one more appropriate than the other.
                                                                    if it's a static action like "View", somewhat ambiguous (TODO: clarify)
                                                                    always prefer the more precise sub-categories below... -->
    "link_action_long" : "action-generic link-long",            <#-- DEPRECATED; TO BE REMOVED -->

    "link_record_id" : "action-nav",                            <#-- the short ID or unique code of a record (1-20 chars): "WS10000", "10000", "ORDER_CANCELLED", etc. -->
    "link_record_id_long" : "action-nav link-long",             <#-- the long ID of a record (more than 20-30 chars), records that do not have single IDs, and IDs with long extraneous words: "WS10000-ITEM10000", "Workspace-Timesheet: TS100000" -->
    "link_record_name" : "action-nav",                          <#-- the name of a record: "My Order 23", "Some Value", "Cancelled", etc. -->
    "link_record_name_long" : "action-nav link-long",           <#-- the long name of a record: "Mr. Title The Ambassador of Germany", etc. -->
    "link_record_idname" : "action-nav",                        <#-- the name and id of a record: "My Order 23 (WS10000)", "WS10000 (My Order 23)" etc. -->
    "link_record_idname_long" : "action-nav link-long",         <#-- long combination of IDs and names: "Mr. John Alberton Smith Junior (ID: 10000) (Group: 20000)" -->
    "link_record_desc" : "action-nav",                          <#-- the description of a record: "Order that was placed by admin", "This is some value", "This means order cancelled", etc. 
                                                                    in general, as soon as a non-action link text contains more than one type of value, and not idname, it should be changed to link_record_desc.
                                                                    however, if it contains a specific action(s), it may be more appropriate as link_action_long. -->
    "link_record_date" : "action-nav",                          <#-- the date of a record (fromDate, thruDate, etc.) -->
    "link_record_number" : "action-nav",                        <#-- the number of a record (index, sequence num, etc.) -->
    "link_record_value" : "action-nav",                         <#-- link containing a value of type not previously listed (or cannot be known statically) -->
    "link_record_value_long" : "action-nav link-long",          <#-- link containing a value of type not previously listed but that may be long (or cannot be known statically) -->
    "link_url" : "",                                            <#-- link containing a URL or location as its text (<a href="http://ofbiz.apache.org">http://ofbiz.apache.org</a>) -->
    "link_text" : "",                                           <#-- link containing any kind of free-form text -->
    "link_image" : "",                                          <#-- link for an image (often omitted) -->
    "link_default" : "",                                        <#-- general link, for anything that does not fall into the above types or cannot be determined.
                                                                    always use the above types instead where possible.
                                                                    usually this will be rarely used and the style here will be left empty. -->
    "link_long" : "link-long",                                  <#-- style to identify long links -->
    
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

  <#-- Lists -->
    "list_inline" : "inline-list",

  <#-- Tile -->
    <#-- tiles-type-based args and styles
         as always, default is also used for individual value fallbacks when missing from specific types. 
         NOTE: currently most of these values are macro args, which may not be straight stylenames (they get mapped to individual styles further below) 
         DEV NOTE: the bg colors may seem like overkill, but they avoid redundancy in the CSS maybe? whatever, more configurable this way -->
    "tile_default_class" : "",  
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
    
    "tile_generic_class" : "",  
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
    
    "tile_gallery1_class" : "",  
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
    "tile_title_default" : "tile-title-default",
    <#-- default overlay styles -->
    "tile_overlay_default" : "tile-overlay-slide-up",    
    <#-- styles for specific overlay types (extensible by theme) -->
    "tile_overlay_slide_up" : "tile-overlay-slide-up",
    <#-- default image styles -->
    "tile_image_default" : "tile-image-cover",
    <#-- styles for specific tile types (extensible by theme) -->
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


