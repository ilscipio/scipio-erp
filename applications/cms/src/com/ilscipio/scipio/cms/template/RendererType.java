package com.ilscipio.scipio.cms.template;

public enum RendererType {
    CMS, // either full cms render or cms render with preview
    CMS_EDITOR, // for small snippet previews in CMS editor
    GENERIC, // generic means non-CMS, either widget or widget-compatible
    UNKNOWN; // unknown == unsupported
}