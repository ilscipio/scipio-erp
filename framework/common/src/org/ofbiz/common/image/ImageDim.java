package org.ofbiz.common.image;

import java.io.Serializable;

import org.ofbiz.base.lang.ThreadSafe;

/**
 * SCIPIO: Simple width x height image dimensions class for return values.
 * Added 2018-08-23.
 */
@SuppressWarnings("serial")
@ThreadSafe
public class ImageDim<N extends Number> implements Serializable {

    protected final N width;
    protected final N height;

    public ImageDim(N width, N height) {
        this.width = width;
        this.height = height;
    }

    /**
     * @return the width
     */
    public N getWidth() {
        return width;
    }

    /**
     * @return the height
     */
    public N getHeight() {
        return height;
    }

    @Override
    public String toString() {
        return width + "x" + height;
    }
}
