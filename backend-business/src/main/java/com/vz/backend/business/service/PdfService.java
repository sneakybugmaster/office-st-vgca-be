package com.vz.backend.business.service;

import com.itextpdf.text.*;
import com.itextpdf.text.pdf.*;
import com.itextpdf.text.pdf.parser.*;
import com.vz.backend.business.dto.CustomMultipartFile;
import com.vz.backend.core.config.Constant;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.UUID;


@Service
public class PdfService {


    @Value("${configs.custom-fonts-folder}")
    private String customFontsFolder;

    @Value("${configs.clerical-seal.font-filename}")
    private String clericalSealFontFilename;

    @Value("${configs.clerical-seal.seal-width-in-cm:5}")
    private float sealWidthInCm;

    @Value("${configs.clerical-seal.seal-height-in-cm:3.5}")
    private float sealHeightInCm;

    @Value("${configs.clerical-seal.seal-font-size:12}")
    private float sealFontSize;

    @Value("${configs.clerical-seal.seal-opacity:0.7}")
    private float sealOpacity;

    @Value("${configs.clerical-seal.seal-line-lmargin:5}")
    private float sealLineLMargin;

    @Value("${configs.clerical-seal.seal-line-indented-lmargin:35}")
    private float sealLineIndentedLMargin;

    @Value("${configs.tmp-file-folder}")
    private String tmpFileFolder;


    public MultipartFile addWatermarkToPDF(MultipartFile inputFile, String docTypeString, String incomingNumberString, Date incomingDate) throws Exception {

        Path tmpPath = Paths.get(tmpFileFolder);

        try (DirectoryStream<Path> stream = Files.newDirectoryStream(tmpPath)) {
            for (Path path : stream) {
                try {
                    if (!Files.isDirectory(path)) {
                        Files.delete(path);
                    }
                } catch (Exception ignored) {
                }
            }
        }

        // Convert MultipartFile to a temporary file
        String randomUUIDString = UUID.randomUUID().toString();

        String inputTempFilename = String.format("input-temp-%s.pdf", randomUUIDString);
        Path inputTempFilePath = tmpPath.resolve(inputTempFilename);

        String outputTempFilename = String.format("output-temp-%s.pdf", randomUUIDString);
        Path outputTempFilePath = tmpPath.resolve(outputTempFilename);

        File inputTempFile = new File(inputTempFilePath.toUri());
        inputFile.transferTo(inputTempFile);

        // Perform watermarking on the temporary file
//        File outputTempFile = File.createTempFile(incomingNumberString, ".pdf");
        File outputTempFile = new File(outputTempFilePath.toUri());
        addWatermarkToPDF(inputTempFile.getAbsolutePath(), outputTempFile.getAbsolutePath(), docTypeString, incomingNumberString, incomingDate);

        // Convert the resulting file to MultipartFile
//        File outputTempFile = new File(outputFilePath);
        FileInputStream fileInputStream = new FileInputStream(outputTempFile);

        return new CustomMultipartFile(fileInputStream, String.format("%s.pdf", incomingNumberString), String.format("%s.pdf", incomingNumberString), "application/pdf");
    }


    public void addWatermarkToPDF(String inputFilePath, String outputFilePath, String docTypeString, String incomingNumberString, Date incomingDate) throws Exception {
        PdfReader reader = new PdfReader(inputFilePath);
        OutputStream outputStream = Files.newOutputStream(Paths.get(outputFilePath));
        PdfStamper stamper = new PdfStamper(reader, outputStream);

        // Add watermark to the first page only
        int pageNum = 1;
        Rectangle pageSize = reader.getPageSizeWithRotation(pageNum);
        PdfContentByte content = stamper.getOverContent(pageNum);

        // Define watermark properties
        Path fontPath = Paths.get(customFontsFolder).resolve(clericalSealFontFilename);
        String fontName = fontPath.toString();
        BaseFont font;
        try {
            font = BaseFont.createFont(fontName, BaseFont.IDENTITY_H, BaseFont.EMBEDDED);
        } catch (Exception e) {
            throw new RuntimeException(String.format("System cannot find font %s", fontName));
        }

        BaseColor fixedBaseColor = new BaseColor(249, 46, 30);
        BaseColor userInputBaseColor = new BaseColor(46, 0, 255);

        Font fixedFont = new Font(font, sealFontSize, Font.BOLD, fixedBaseColor);
        Font userInputFont = new Font(font, sealFontSize, Font.BOLD, userInputBaseColor);

        // Create watermark content
        Phrase orgNamePhrase = new Phrase(Constant.ORG_NAME_UPPER, new Font(font, sealFontSize + 1, Font.BOLD, fixedBaseColor));

        Phrase docTypePhrase = new Phrase(docTypeString, new Font(font, sealFontSize + 2, Font.BOLD, fixedBaseColor));

        Phrase incomingNumberPhrase = new Phrase("Số: ", fixedFont);
        Chunk incomingNumberChunk = new Chunk(incomingNumberString, userInputFont);
        incomingNumberPhrase.add(incomingNumberChunk);

        Calendar calendar = new GregorianCalendar();
        calendar.setTime(incomingDate);

        String incomingDateDayString = String.valueOf(calendar.get(Calendar.DATE));
        String incomingDateMonthString = String.valueOf(calendar.get(Calendar.MONTH) + 1);
        String incomingDateYearString = String.valueOf(calendar.get(Calendar.YEAR));


        String incomingDateDayStringPadded = StringUtils.leftPad(incomingDateDayString, 2, '0');
        String incomingDateMonthStringPadded = StringUtils.leftPad(incomingDateMonthString, 2, '0');

        Phrase incomingDatePhrase = new Phrase("Ngày: ", fixedFont);
        Chunk incomingDateChunkDate = new Chunk(incomingDateDayStringPadded, userInputFont);
        Chunk incomingDateChunkSeparator = new Chunk("/", new Font(font, sealFontSize, Font.NORMAL, fixedBaseColor));
        Chunk incomingDateChunkMonth = new Chunk(incomingDateMonthStringPadded, userInputFont);
        Chunk incomingDateChunkYear = new Chunk(incomingDateYearString, userInputFont);
        incomingDatePhrase.add(incomingDateChunkDate);
        incomingDatePhrase.add(incomingDateChunkSeparator);
        incomingDatePhrase.add(incomingDateChunkMonth);
        incomingDatePhrase.add(incomingDateChunkSeparator);
        incomingDatePhrase.add(incomingDateChunkYear);

        Phrase phraseLine4 = new Phrase("Chuyển:............................", fixedFont);
        Phrase phraseLine5 = new Phrase("Số và ký hiệu HS:.............", fixedFont);


        // Calculate watermark position
        float x = pageSize.getWidth() * 0.02f;  // 2% width from left
        float y = pageSize.getHeight() * 0.65f;  // 35% height from top

        // Seal width
        float width = sealWidthInCm * Constant.CM_TO_POINT_CONVERSION_RATE;
        float sealFullHeight = sealHeightInCm * Constant.CM_TO_POINT_CONVERSION_RATE;
        // Seal upper part height
        float upperBlockHeight = sealFullHeight * 0.2f;
        // Seal lower part height
        float lowerBlockHeight = sealFullHeight * 0.8f;

        float leftX = pageSize.getWidth() * 0.02f;
        float rightX = pageSize.getWidth() * 0.98f;
        float topY = pageSize.getHeight() * 0.9f;
        float botY = pageSize.getHeight() * 0.6f;
        float stepX = (rightX - leftX) / 5;
        float stepY = (topY - botY) / 5;

        Rectangle bestRectangle = rectangleWithLowestAmountOfText(reader, 1, leftX, rightX, topY, botY, stepX, stepY, width, sealFullHeight);
        if (bestRectangle != null) {
            x = bestRectangle.getLeft();
            y = bestRectangle.getBottom();
        }

        content.saveState();

        // Set sealOpacity for text and border
        PdfGState gState = new PdfGState();
        gState.setFillOpacity(sealOpacity);
        gState.setStrokeOpacity(sealOpacity);
        content.setGState(gState);

        // Set color and sealOpacity for borders
        content.setLineWidth(2f);

        // Draw upper border
        content.setColorStroke(fixedBaseColor);
        content.rectangle(x, y + lowerBlockHeight, width, upperBlockHeight);
        content.stroke();

        // Draw lower border
        content.setColorStroke(fixedBaseColor);
        content.rectangle(x, y, width, lowerBlockHeight);
        content.stroke();

        float lineOffset = lowerBlockHeight * 0.0625f;

        // Calculate the center position for alignment of each line
        float centerX = x + width / 2;
        float centerYOrgNameLine = y + lowerBlockHeight + lineOffset;
        float centerYDocTypeLine = y + lowerBlockHeight * 0.75f - lineOffset;
        float centerYIncomingNumberLine = y + lowerBlockHeight * 0.875f - lineOffset;
        float centerYIncomingDateLine = y + lowerBlockHeight * 0.625f - lineOffset;
        float centerYRecipientLine = y + lowerBlockHeight * 0.375f - lineOffset;
        float centerYNumberOrSignLine = y + lowerBlockHeight * 0.125f - lineOffset;

        float normalLineMargin = x + sealLineLMargin;
        float indentedLineMargin = x + sealLineIndentedLMargin;

        // Draw the watermark text
        ColumnText.showTextAligned(content, Element.ALIGN_CENTER, orgNamePhrase, centerX, centerYOrgNameLine, 0);
        ColumnText.showTextAligned(content, Element.ALIGN_LEFT, docTypePhrase, normalLineMargin, centerYDocTypeLine, 0);
        ColumnText.showTextAligned(content, Element.ALIGN_LEFT, incomingNumberPhrase, indentedLineMargin, centerYIncomingNumberLine, 0);
        ColumnText.showTextAligned(content, Element.ALIGN_LEFT, incomingDatePhrase, indentedLineMargin, centerYIncomingDateLine, 0);
        ColumnText.showTextAligned(content, Element.ALIGN_LEFT, phraseLine4, normalLineMargin, centerYRecipientLine, 0);
        ColumnText.showTextAligned(content, Element.ALIGN_LEFT, phraseLine5, normalLineMargin, centerYNumberOrSignLine, 0);

        content.restoreState();

        stamper.close();
        reader.close();
        outputStream.close();
    }

    private Rectangle rectangleWithLowestAmountOfText(PdfReader reader, int pageNumber, float leftX, float rightX, float topY, float botY, float stepX, float stepY, float width, float height) throws IOException {
        float lowestDensity = Float.MAX_VALUE;
        Rectangle bestRectangle = null;
        for (float x = leftX; x <= rightX - width; x += stepX) {
            for (float y = topY; y >= botY + height; y -= stepY) {
                Rectangle currentRectangle = new Rectangle(x, y - height, x + width, y);
                float density = calculateTextDensity(reader, currentRectangle, pageNumber);
                if (density < lowestDensity) {
                    lowestDensity = density;
                    bestRectangle = currentRectangle;
                    if (lowestDensity == 0f) {
                        break;
                    }
                }
            }
        }
        return bestRectangle;

    }

    private float calculateTextDensity(PdfReader reader, Rectangle rect, int pageNumber) throws IOException {
        RenderFilter filter = new RegionTextRenderFilter(rect);
        TextExtractionStrategy strategy = new FilteredTextRenderListener(new LocationTextExtractionStrategy(), filter);
        // calculate text density in the rectangle
        String text = com.vz.backend.util.StringUtils.cutAllSpace( PdfTextExtractor.getTextFromPage(reader, pageNumber, strategy));
        if (text == null) {
            text = "";
        }

        return (float) text.length() / rect.getWidth() / rect.getHeight();
    }


}
