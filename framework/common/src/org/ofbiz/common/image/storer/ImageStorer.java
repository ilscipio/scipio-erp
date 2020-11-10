package org.ofbiz.common.image.storer;

import org.ofbiz.common.image.ImageOp;
import org.ofbiz.entity.Delegator;

import java.awt.image.RenderedImage;
import java.io.IOException;
import java.util.Map;

/**
 * Image storer interface, based on ImageIO.write.
 * Configured in imageops.properties.
 * TODO: This should probably implement ImageOp interface if it can be refitted.
 */
public interface ImageStorer extends ImageOp {

    boolean write(RenderedImage im, String formatName, Object output, String mediaProfile, Map<String, Object> options, Delegator delegator) throws IOException;

    boolean isApplicable(RenderedImage im, String formatName, Object output, String mediaProfile, Map<String, Object> options, Delegator delegator);

    interface ImageStorerFactory extends ImageOp.ImageOpFactory<ImageStorer> {
    }

}
