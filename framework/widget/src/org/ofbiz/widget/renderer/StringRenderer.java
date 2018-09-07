package org.ofbiz.widget.renderer;

/**
 * SCIPIO: New base interface for the *StringRenderers.
 * Added 2018-09-06.
 */
public interface StringRenderer {

    /**
     * SCIPIO: Returns renderer name in use in current render.
     * <p>
     * Common names: "html", "xml", "text", "csv", "xsl-fo", "email" (new in Scipio).
     */
    public String getRendererName();

}
