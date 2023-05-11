package com.vz.backend.core.service;

import com.itextpdf.text.Document;
import com.itextpdf.text.Image;
import com.itextpdf.text.pdf.PdfWriter;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.StringUtils;
import org.apache.commons.io.FilenameUtils;
import org.apache.pdfbox.pdmodel.PDDocument;
import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.docx4j.Docx4J;
import org.docx4j.openpackaging.packages.WordprocessingMLPackage;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

@Service
public class FileService {

	public boolean isDocFile(MultipartFile file) {
		String originName = file.getOriginalFilename();
		
		if (originName == null) {
			return false;
		}
		return originName.endsWith(Constant.DOCX) || originName.endsWith(Constant.DOCXS)
				|| originName.endsWith(Constant.DOCS)
				|| originName.endsWith(Constant.DOC)
				|| originName.endsWith(Constant.ODT)
				|| originName.endsWith(Constant.ODTS);
	}

	public boolean isPdfFile(MultipartFile file) {
		return file.getOriginalFilename().endsWith(Constant.PDF) || file.getOriginalFilename().endsWith(Constant.PDFS);
	}

	public boolean isImgFile(MultipartFile file) {
		return file.getOriginalFilename().endsWith(Constant.JPGS) || file.getOriginalFilename().endsWith(Constant.JPG)
				|| file.getOriginalFilename().endsWith(Constant.PNG)
				|| file.getOriginalFilename().endsWith(Constant.PNGS);
	}
	
	public File downloadFile2(InputStream inputStream, String fileName) {
		try {
			Path root = Paths.get("uploads");
			Path path = root.resolve(fileName);
			Files.copy(inputStream, path, StandardCopyOption.REPLACE_EXISTING);
			Resource resource = new UrlResource(path.toUri());
			return resource.getFile();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}
	
    public Resource docxToPdf(MultipartFile file) {
        try (InputStream is = file.getInputStream()) {
            File f = downloadFile2(is, FilenameUtils.removeExtension(file.getOriginalFilename()) + Constant.PDFS);
            WordprocessingMLPackage wordMLPackage = WordprocessingMLPackage.load(is);
            try (OutputStream out = new FileOutputStream(f)) {
                Docx4J.toPDF(wordMLPackage, out);
                Path fs = Paths.get(StringUtils.decodeFromUrl(f.getAbsolutePath()));
                return new UrlResource(fs.toUri());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        throw new RestExceptionHandler(Message.NO_INPUT_DATA);
    }

    public Resource imgToPdf(MultipartFile file) {
        try (InputStream is = file.getInputStream()){
            File f = downloadFile2(is, FilenameUtils.removeExtension(file.getOriginalFilename()) + Constant.PDFS);
            Image image = Image.getInstance(file.getBytes());

            //resize
            if (image.getWidth() > 500) {
                image.scaleToFit(500, 800);
            }

            Document document = new Document();
            PdfWriter writer = null;
            try (FileOutputStream fos = new FileOutputStream(f)) {
                writer = PdfWriter.getInstance(document, fos);
                writer.open();
                document.open();
                document.add(image);
            } finally {
                document.close();
                if (writer != null) {
                    writer.close();
                }
            }
            Path fs = Paths.get(StringUtils.decodeFromUrl(f.getAbsolutePath()));
            return new UrlResource(fs.toUri());
        } catch (Exception e) {
            e.printStackTrace();
        }
        throw new RestExceptionHandler(Message.NO_INPUT_DATA);
    }
	
	public Resource convertToPdf(MultipartFile file) {
		Resource rs = file.getResource();
		if (isDocFile(file)) {
			rs = docxToPdf(file);
		}

		if (isImgFile(file)) {
			rs = imgToPdf(file);
		}

		return rs;
	}
	
	public Resource getResource(MultipartFile file) {
		Resource rs = file.getResource();
		if (!isPdfFile(file)) {
			rs = convertToPdf(file);
		}
		return rs;
	}
	
	/**
	 * Get page size of pdf file
	 * 
	 * @param input
	 * @return
	 */
    private int getPageSize(InputStream input) {
        int pageSize = 0;
        try (PDDocument pdDoc = PDDocument.load(input)) {
            pageSize = pdDoc.getNumberOfPages();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return pageSize;
    }

	/**
	 * Get page size of (doc/docx/pdf) file
	 * 
	 * @param input
	 * @param type
	 * @return
	 */
    private int getPageSize(InputStream input, String type) {
        HWPFDocument doc = null;
        XWPFDocument docx = null;
        int page = 0;
        try {
            switch (type) {
                case "doc":
                case "DOC":
                    doc = new HWPFDocument(input);
                    page = doc.getSummaryInformation().getPageCount();
                    break;
                case "docx":
                case "DOCX":
                    docx = new XWPFDocument(input);
                    page = docx.getProperties().getExtendedProperties().getUnderlyingProperties().getPages();
                    break;
                case "pdf":
                case "PDF":
                    page = getPageSize(input);
                    break;
                default:
                    break;
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
        return page;
    }

    public int getPageSize(MultipartFile file) {
        if (file == null) {
            return 0;
        }
        String type = FilenameUtils.getExtension(file.getOriginalFilename());
        int page = 0;
        try (InputStream fis = file.getInputStream()) {
            page = getPageSize(fis, type);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return page;
    }
}
