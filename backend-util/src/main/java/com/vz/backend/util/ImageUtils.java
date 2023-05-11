package com.vz.backend.util;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.imageio.ImageIO;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.imgscalr.Scalr;
import org.imgscalr.Scalr.Method;
import org.imgscalr.Scalr.Mode;

public final class ImageUtils {
	public static final String SIZE_TYPE_THUMBNAIL = "THUMB";
	public static final String SIZE_TYPE_MEDIUM = "MEDIUM";
	public static final String SIZE_TYPE_STANDARD = "STANDARD";

	private static final double THUMB_WIDTH_PREFERRED = 150;
	private static final double THUMB_HEIGHT_PREFERRED = 150;

	private static final double MEDIUM_WIDTH_PREFERRED = 300;
	private static final double MEDIUM_HEIGHT_PREFERRED = 300;

	private static final double STANDARD_WIDTH_PREFERRED = 450;
	private static final double STANDARD_HEIGHT_PREFERRED = 450;

	private static Log log = LogFactory.getLog(ImageUtils.class);

	public static byte[] resizeImage(byte[] result, String sizeType) throws IOException {
		if (sizeType != null) {
			if (sizeType.equalsIgnoreCase(SIZE_TYPE_THUMBNAIL)) {
				return resizeImage(result, THUMB_WIDTH_PREFERRED, THUMB_HEIGHT_PREFERRED);
			} else if (sizeType.equalsIgnoreCase(SIZE_TYPE_MEDIUM)) {
				return resizeImage(result, MEDIUM_WIDTH_PREFERRED, MEDIUM_HEIGHT_PREFERRED);
			} else if (sizeType.equalsIgnoreCase(SIZE_TYPE_STANDARD)) {
				return resizeImage(result, STANDARD_WIDTH_PREFERRED, STANDARD_HEIGHT_PREFERRED);
			}
		}

		return result;
	}

	public static byte[] resizeImage(byte[] result, double w, double h) throws IOException {
		try (InputStream in = new ByteArrayInputStream(result)){
			byte[] imageInByte;
			BufferedImage bimg = ImageIO.read(in);
			if (bimg == null) {
				return null;
			}

			BufferedImage newBimg = new BufferedImage(bimg.getWidth(), bimg.getHeight(), BufferedImage.TYPE_INT_RGB);
			newBimg.createGraphics().drawImage(bimg, 0, 0, Color.WHITE, null);

			BufferedImage imageToSave = new BufferedImage(newBimg.getWidth(), newBimg.getHeight(),
					BufferedImage.TYPE_INT_RGB);
			Graphics g = imageToSave.getGraphics();
			g.drawImage(newBimg, 0, 0, null);

			imageToSave = Scalr.resize(imageToSave, Method.AUTOMATIC, Mode.FIT_EXACT, (int) w, (int) h);

			try (ByteArrayOutputStream baos = new ByteArrayOutputStream()) {
				ImageIO.write(imageToSave, "jpg", baos);
				baos.flush();
				imageInByte = baos.toByteArray();
				return imageInByte;
			}
		} catch (IOException ex) {
			log.info(ex);
			return result;
		}
	}

}
