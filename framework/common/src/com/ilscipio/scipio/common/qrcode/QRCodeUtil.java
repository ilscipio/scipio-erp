package com.ilscipio.scipio.common.qrcode;

import java.awt.AlphaComposite;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.File;

import javax.imageio.ImageIO;

import org.apache.commons.io.FilenameUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.FileUtil;

import net.glxn.qrgen.javase.QRCode;

/**
 * SCIPIO qrcode servlet - NOT associated with stock qrcode at current time
 * (org.ofbiz.common.qrcode).
 */
public class QRCodeUtil {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * Renders overlay over QRCode
     * Calls {@link QRCodeUtil#drawLogo(File, String) drawLogo}.
     * */
    public static File drawLogo(QRCode qrCode,String logoPath){
        try{
            return drawLogo(qrCode.file(),logoPath);
        }catch(Exception e){
            Debug.logError("Error while drawing logo in QR Code", module);
        }
        return null;
    }


    /**
     * Renders overlay over QRCode
     * @param qrCode Original qrCode File
     * @param logoPath path to overlaying image
     * */
    public static File drawLogo(File qrCode,String logoPath){
        try{
            BufferedImage qrImage = ImageIO.read(qrCode);
            BufferedImage logo = ImageIO.read(FileUtil.getFile(logoPath));
            double determineImageScale = determineImageScale(logo.getWidth(), logo.getHeight(), qrImage.getWidth(),qrImage.getHeight());
            BufferedImage overlay = resizeImg(logo,determineImageScale);
            //BufferedImage overlay = logo;

            int deltaHeight = qrImage.getHeight() - overlay.getHeight();
            int deltaWidth  = qrImage.getWidth()  - overlay.getWidth();

            BufferedImage combined = new BufferedImage(qrImage.getWidth(),qrImage.getHeight(), BufferedImage.TYPE_INT_ARGB);
            Graphics g = combined.getGraphics();
            g.drawImage(qrImage, 0, 0, null);
            ((Graphics2D) g).setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 1f));
            g.drawImage(overlay, Math.round(deltaWidth/2), Math.round(deltaHeight/2), null);
            String extension = FilenameUtils.getExtension(qrCode.getName());
            File imageFile = File.createTempFile("QRCode", "." + extension);
            ImageIO.write(combined, "PNG", imageFile);
            imageFile.deleteOnExit();
            return imageFile;
        }catch(Exception e){
            Debug.logError("Error while drawing logo in QR Code", module);
        }
        return null;

    }

    /**
     * Function to scale an image down, requires scaledWidth (determined via {@link #determineImageScale(int, int, int, int)}
     * */
    public static BufferedImage resizeImg(BufferedImage image, double scaledWidth)
    {
        Image scaledImage = image.getScaledInstance((int) (image.getWidth() * scaledWidth), (int) (image.getHeight() * scaledWidth), Image.SCALE_SMOOTH);
        BufferedImage bufferedImage = new BufferedImage(
        scaledImage.getWidth(null),
        scaledImage.getHeight(null),
        BufferedImage.TYPE_INT_RGB
        );
        Graphics g = bufferedImage.createGraphics();
        g.drawImage(scaledImage, 0, 0, null);
        g.dispose();
        return bufferedImage;
   }


    /**
     * Function to determine proper aspect ratio
     * */
    private static double determineImageScale(int sourceWidth, int sourceHeight, int targetWidth, int targetHeight) {
        double scalex = (double) targetWidth / sourceWidth;
        double scaley = (double) targetHeight / sourceHeight;
        return Math.min(scalex, scaley);
        }
}
