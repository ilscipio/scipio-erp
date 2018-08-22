package org.ofbiz.common.qrcode;

import java.awt.AlphaComposite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import org.ofbiz.base.util.FileUtil;

/**
 * SCIPIO: qrcode util.
 */
public class QRCodeUtil {
    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Renders overlay over QRCode (original Scipio method).
     */
    public static File drawLogoToFile(BufferedImage qrImage, String format, String logoPath) throws IOException {
        BufferedImage combined = drawLogo(qrImage, ImageIO.read(FileUtil.getFile(logoPath)));
        File imageFile = File.createTempFile("QRCode", "." + format);
        ImageIO.write(combined, "PNG", imageFile);
        imageFile.deleteOnExit();
        return imageFile;
    }

    /**
     * Renders overlay over QRCode (original Scipio method).
     */
    public static BufferedImage drawLogo(BufferedImage qrImage, BufferedImage logoImage) {
        double determineImageScale = determineImageScale(logoImage.getWidth(), logoImage.getHeight(), 
                qrImage.getWidth(), qrImage.getHeight());
        BufferedImage overlay = resizeImg(logoImage, determineImageScale);

        int deltaHeight = qrImage.getHeight() - overlay.getHeight();
        int deltaWidth  = qrImage.getWidth()  - overlay.getWidth();

        BufferedImage combined = new BufferedImage(qrImage.getWidth(),qrImage.getHeight(), BufferedImage.TYPE_INT_ARGB);
        Graphics g = combined.getGraphics();
        g.drawImage(qrImage, 0, 0, null);
        ((Graphics2D) g).setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1f));
        g.drawImage(overlay, Math.round(deltaWidth/2), Math.round(deltaHeight/2), null);
        return combined;
    }
    
    /**
     * Function to scale an image down, requires scaledWidth (original Scipio method).
     * */
    public static BufferedImage resizeImg(BufferedImage image, double scaledWidth) {
        Image scaledImage = image.getScaledInstance((int) (image.getWidth() * scaledWidth), (int) (image.getHeight() * scaledWidth), Image.SCALE_SMOOTH);
        BufferedImage bufferedImage = new BufferedImage(
                scaledImage.getWidth(null),
                scaledImage.getHeight(null),
                BufferedImage.TYPE_INT_RGB);
        Graphics g = bufferedImage.createGraphics();
        g.drawImage(scaledImage, 0, 0, null);
        g.dispose();
        return bufferedImage;
    }

    /**
     * Function to determine proper aspect ratio (original Scipio method).
     * */
    private static double determineImageScale(int sourceWidth, int sourceHeight, int targetWidth, int targetHeight) {
        double scalex = (double) targetWidth / sourceWidth;
        double scaley = (double) targetHeight / sourceHeight;
        return Math.min(scalex, scaley);
    }
}
