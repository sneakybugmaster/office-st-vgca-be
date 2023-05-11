package com.vz.backend.core.service;

import java.io.*;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.stream.Stream;

import com.vz.backend.core.dto.DocIssuedData;
import org.apache.commons.lang.ArrayUtils;
import org.apache.poi.xwpf.usermodel.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.util.UriUtils;

import com.vz.backend.core.config.Constant;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class FilesStorageService {
	@Autowired
	private FTPService ftpService;
	
	@Value("${ftpConfigs.active:false}")
	private boolean ftp;

	public static final String UTF8 = "UTF8";

	private final Path hsPath = Paths.get("hspath");
	private final Path root = Paths.get("uploads");
	private final Path deletePath = Paths.get("delete");
	private final Path tmpPath = Paths.get("tmpFiles");
	private final Path encryptFile = Paths.get(Constant.ENCRYPT_FILE_PATH);
	// private final Path docInternalPath = Paths.get("docInternal");
	private static final String SLASH = "/";
	private static final String UPLOADS = "uploads";
	private static final String HSPATH = "hspath";
	private static final String DELETE = "delete";
	private static final String TMPFILES = "tmpFiles";
	private static final String DOCINTERNAL = "docInternal";
	private boolean isFillNumberOrSign = false;
	private boolean isFillIssuedDate = false;
	public FilesStorageService() {
		if (!Files.exists(root) || !Files.exists(deletePath) || !Files.exists(hsPath) || !Files.exists(tmpPath)
				|| !Files.exists(encryptFile)) {
			this.init();
		}
	}

	public void init() {
		try {
			Files.createDirectories(root);
			Files.createDirectories(hsPath);
			Files.createDirectories(deletePath);
			Files.createDirectories(tmpPath);
			Files.createDirectories(encryptFile);
			// Files.createDirectories(docInternalPath);
		} catch (IOException e) {
			throw new RuntimeException("Could not initialize folder for upload!");
		}
	}

	public static String origin(String parse) {
		if (parse == null) {
			return null;
		}
		return parse.replaceAll("__\\d+$", "");
	}

	public static String parse(String origin) {
		if (origin == null) {
			return null;
		}
		return origin + "__" + new Date().getTime();
	}

	// save file into `rootPath` folder
	public void saveToSystem(MultipartFile file, String fileName) {
		saveToSystem(file, fileName, root);
	}

	// save file into `rootPath` folder
	public void saveToSystem(MultipartFile file, String fileName, Path rootPath) {
		try (InputStream is = file.getInputStream()){
			save(is, fileName, rootPath);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void save(InputStream inputStream, String fileName, Path rootPath) {
		if (ftp) {
			try {
				String dir = getDir(rootPath);
				ftpService.upload(fileName, dir, inputStream);
			} catch (Exception e) {
				e.printStackTrace();
			}
			return;
		}

		Path path = resolveEncode(fileName, rootPath);
		try {
			Files.copy(inputStream, path, StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException e) {
		}
	}

	public static String getFileName(MultipartFile file) {
		return file.getOriginalFilename();
	}

	public String save(MultipartFile file) {
		return save(file, root);
	}

	public String saveHs(MultipartFile file) {
		return save(file, hsPath);
	}

	public String saveDocInternal(MultipartFile file) {
		String fileName = FilesStorageService.getFileName(file);
		fileName = FilesStorageService.parse(fileName);
		saveToSystem(file, fileName, root);
		return fileName;
	}

	public String saveDocInternal(MultipartFile file, String fileName) {
		saveToSystem(file, fileName, root);
		return fileName;
	}

	public String save(MultipartFile file, Path path) {

		String filename = FilesStorageService.parse(FilesStorageService.getFileName(file));
		log.error("filename: {}", filename);
		saveToSystem(file, filename, path);
		return filename;
	}

	// replace
	public String replace(MultipartFile file, String fileName) {
		fileName = StringUtils.cleanPath(fileName);
		deleteFile(fileName);
		String newfileName = save(file);
		log.info("Replace {} by {}", fileName, newfileName);
		return newfileName;
	}

	public String replaces(MultipartFile file, String fileName) {
		fileName = StringUtils.cleanPath(fileName);
		deleteFile(fileName);
		saveToSystem(file, fileName);
		return fileName;
	}

	public Resource load(String filename) {
		return load(filename, root);
	}

	public Resource loadHs(String filename) {
		return load(filename, hsPath);
	}

	public Resource loadDocInternal(String filename) {
		return load(filename, root);
	}

	public Resource load(String fileName, Path rootPath) {
		if (ftp) {
			String dir = getDir(rootPath);
			return ftpService.load(fileName, dir);
		}

		try {
			Path file = this.resolveEncode(fileName, rootPath);
			if (file == null) {
				return null;
			}
			Resource resource = new UrlResource(file.toUri());

			if (resource.exists() || resource.isReadable()) {
				return resource;
			}
		} catch (MalformedURLException e) {
			throw new RuntimeException("Error: " + e.getMessage());
		}

		return null;
	}

	private Path resolveEncode(String fileName, Path rootPath) {
		try {
			return rootPath.resolve(fileName);
		} catch (InvalidPathException e) {
			return rootPath.resolve(UriUtils.encode(fileName, UTF8));
		} catch (Exception e) {
			e.printStackTrace();
			log.error("Cannot load file {} on {}", fileName, rootPath.toString());
			return null;
		}

	}

	public Path getPath(String fileName) {
		fileName = StringUtils.cleanPath(fileName);
		return resolveEncode(fileName, root);
	}

	public File getFile(String fileName) {
		return getPath(fileName).toFile();
	}

	public boolean existFile(String fileName) {
		try {
			Path file = resolveEncode(fileName, root);
			if (Files.exists(file)) {
				return true;
			}
		} catch (Exception e) {
		}
		return false;
	}

	public boolean replaceFiles(String fileName, String replaceToFile) {
		try {
			Files.move(resolveEncode(fileName, root), resolveEncode(replaceToFile, root), StandardCopyOption.REPLACE_EXISTING);
			return true;
		} catch (IOException e) {
			return false;
		}
	}

	public boolean deleteFile(String fileName) {
		fileName = StringUtils.cleanPath(fileName);
		if (ftp) {
			return ftpService.move(fileName, UPLOADS, DELETE);
		}

		try {
			Files.move(
					resolveEncode(fileName, root),
					resolveEncode(fileName, deletePath),
					StandardCopyOption.REPLACE_EXISTING);
			return true;
		} catch (IOException e) {
			return false;
		}
	}

	public boolean copyFile(String fileNameFrom, String fileNameTo) {
		if (ftp) {
			return ftpService.copy(fileNameFrom, fileNameTo);
		}

		try {
			Files.copy(resolveEncode(fileNameFrom, root), resolveEncode(fileNameTo, root),
					StandardCopyOption.REPLACE_EXISTING);
			return true;
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
	}

	//	public void deleteAll() {
	//		FileSystemUtils.deleteRecursively(root.toFile());
	//	}

	public Stream<Path> loadAll() {
		try {
			return Files.walk(this.root, 1).filter(path -> !path.equals(this.root)).map(this.root::relativize);
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException("Could not load the files!");
		}
	}

	public void saveTmpFile(MultipartFile file, String fileName) {
		try (InputStream inputStream = file.getInputStream()){
			if (ftp) {
				ftpService.upload(fileName, TMPFILES, inputStream);
				return;
			}

			// FileUtils.cleanDirectory(tmpPath.toFile());
			Path path = resolveEncode(fileName, tmpPath);
			Files.copy(inputStream, path, StandardCopyOption.REPLACE_EXISTING);
		} catch (IOException e) {
			log.error(e.getMessage(), e);
		}
	}

	public Resource loadTmpFile(String filename) {
		if (ftp) {
			return ftpService.load(filename, TMPFILES);
		}

		try {
			Path file = resolveEncode(filename, tmpPath);
			Resource resource = new UrlResource(file.toUri());
			if (resource.exists() || resource.isReadable()) {
				return resource;
			}
		} catch (IOException e) {
			throw new RuntimeException("Error: " + e.getMessage());
		}
		return null;
	}

	public void deleteTmpFile(String filename) {
		if (ftp) {
			ftpService.delete(filename, TMPFILES);
			return;
		}

		try {
			Path file = resolveEncode(filename, tmpPath);
			Files.deleteIfExists(file);
		} catch (IOException e) {
			throw new RuntimeException("Error: " + e.getMessage());
		}
	}

	public void delete(String fileName) {
		fileName = StringUtils.cleanPath(fileName);
		if (ftp) {
			ftpService.delete(fileName, UPLOADS);
			return;
		}

		try {
			Path file = resolveEncode(fileName, root);
			Files.delete(file);
		} catch (IOException e) {
			throw new RuntimeException("Error: " + e.getMessage());
		}
	}

	public void delete(Path path, String fileName) {
		if (ftp) {
			String dir = getDir(path);
			ftpService.delete(fileName, dir);
			return;
		}

		try {
			Path file = path.resolve(fileName);
			Files.delete(file);
		} catch (IOException e) {
			e.printStackTrace();
			throw new RuntimeException("Error: " + e.getMessage());
		}
	}
	
	/**
	 * Get dir by path
	 * @param path
	 * @return
	 */
	public String getDir(Path path) {
		if (path == null)
			return UPLOADS;
		Path paths = path.getParent();
		if (paths == null)
			return path.toString();
		String[] dirs = paths.toString().split(SLASH);
		if (ArrayUtils.isEmpty(dirs))
			return UPLOADS;
		String dir = dirs[dirs.length - 1];
		if (dir == null)
			return UPLOADS;
		switch (dir) {
		case UPLOADS:
		case DELETE:
		case HSPATH:
		case TMPFILES:
		case DOCINTERNAL:
		case Constant.ENCRYPT_FILE_PATH:
			return dir;
		default:
			return UPLOADS;
		}
	}
	
	public void delete(Path path, List<String> fileNames) {
		for (String i : fileNames) {
			delete(path, i);
		}
	}

	public void printDocIssuedData(DocIssuedData docIssuedData, String fileName) {
		Path tmpPath = null;
		Path fileNamePath = null;
		try {
			String tmp = fileName + "__tmp";
			tmpPath = root.resolve(tmp);
			fileNamePath = root.resolve(fileName);
			if (ftp) {
				try (OutputStream outputStreamTmp = new FileOutputStream(fileNamePath.toFile())) {
					if (ftpService.download(fileName, "UPLOADS", outputStreamTmp)) {
						Files.copy(fileNamePath, tmpPath, StandardCopyOption.REPLACE_EXISTING);
					}
				}
			} else {
				fileNamePath = root.resolve(fileName);
				Files.copy(fileNamePath, tmpPath, StandardCopyOption.REPLACE_EXISTING);
			}

			try (InputStream inputStream = Files.newInputStream(tmpPath);
				 OutputStream outputStream = new FileOutputStream(fileNamePath.toFile())) {
				this.printDocIssuedData(docIssuedData, inputStream, outputStream);
				if (ftp) {
					// xoa file fpt
					ftpService.delete(fileName, UPLOADS);

					// uploads to remote
					try (InputStream is = Files.newInputStream(fileNamePath)) {
						ftpService.upload(fileName, UPLOADS, is);
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			try {
				if (tmpPath != null) {
					Files.delete(tmpPath);
				}

				if (ftp && fileNamePath != null) {
					Files.delete(fileNamePath);
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}
	private void printDocIssuedData(DocIssuedData docIssuedData, InputStream inputStream, OutputStream outputStream)
			throws IOException {
		this.isFillNumberOrSign = false;
		this.isFillIssuedDate = false;
		XWPFDocument doc = new XWPFDocument(inputStream);
		for (XWPFParagraph p : doc.getParagraphs()) {
			if (this.isFillNumberOrSign && this.isFillIssuedDate)
				break;
			String numberOrSign = fillNumberOrSign(p.getText(), docIssuedData);
			String dateIssued = fillDateIssued(p.getText(), docIssuedData);
			if (numberOrSign != null) {
				changeText(p, numberOrSign);
			}

			if (dateIssued != null) {
				changeText(p, dateIssued);
			}
		}

		for (XWPFTable tb : doc.getTables()) {
			for (XWPFTableRow row : tb.getRows()) {
				for (XWPFTableCell cell : row.getTableCells()) {
					for (XWPFParagraph p : cell.getParagraphs()) {
//						if (this.isFillNumberOrSign && this.isFillIssuedDate)
//							break;
						String numberOrSign = fillNumberOrSign(p.getText(), docIssuedData);

						String dateIssued = fillDateIssued(p.getText(), docIssuedData);
						if (numberOrSign != null) {
							changeText(p, numberOrSign);
						}

						if (dateIssued != null) {
							changeText(p, dateIssued);
						}
					}
				}
			}

		}
		doc.write(outputStream);
	}

	public String fillNumberOrSign(String text, DocIssuedData docIssuedData) {
		if (this.isFillNumberOrSign || StringUtils.isEmpty(text) || text.length() > 100)
			return null;
		final String SO = "số";
		final String KI = "kí";
		final String HIEU = "hiệu";
		final String KY = "ký";
		final String COLON = ":";
		final String COMON = ",";
		String label = null;
		String[] arr = text.toLowerCase().trim().split(" ");
		List<String> list = new ArrayList<>();
		for (int i = 0; i < arr.length; i++) {
			String tmp = arr[i];
			String[] tmpArr = null;
			if (tmp.contains(COMON)) {
				tmpArr = tmp.split(COMON);
				if (tmpArr.length > 0) {
					list.addAll(Arrays.asList(tmpArr));
				}
			} else if (tmp.contains(COLON)) {
				tmpArr = tmp.split(COLON);
				if (tmpArr.length > 0) {
					list.addAll(Arrays.asList(tmpArr));
				}
				list.add(COLON);
			} else {
				list.add(tmp);
			}
		}

		// For case no label
		if (list.isEmpty())
			return null;

		// For case had data -> don't fill data
		if (!COLON.equals(list.get(list.size() - 1)) && !HIEU.equals(list.get(list.size() - 1)) && !SO.equals(list.get(0)))
			return null;

		// For case hasn't fill data yet
		int point = 0;
		for (String str : list) {
			switch (str) {
				case SO:
					point += 1;
					break;
				case KI:
					point += 2;
					break;
				case HIEU:
					point += 3;
					break;
				case KY:
					point += 4;
					break;
				default:
					break;
			}
		}

		switch (point) {
			case 1:
				label = "Số : ";
				break;
			case 6:
				label = "Số, kí hiệu : ";
				break;
			case 8:
				label = "Số, ký hiệu : ";
				break;
			case 5:
				label = "Kí hiệu : ";
				break;
			case 7:
				label = "Ký hiệu : ";
				break;
			default:
				return null;
		}
		this.isFillNumberOrSign = true;
		return label + docIssuedData.getNumberOrSign();
	}


	private String fillDateIssued(String text, DocIssuedData data) {
//		if (this.isFillIssuedDate)
//			return null;
		final String NGAY = "ngày";
		final String THANG = "tháng";
		final String NAM = "năm";

		// Input condition
		if (!text.toLowerCase().contains(NGAY) || !text.toLowerCase().contains(THANG)
				|| !text.toLowerCase().contains(NAM) || text.length() > 100) {
			return null;
		}

		String[] arr = text.trim().split(",");
		if (arr.length == 0)
			return text;
		String address = arr[0].trim().equalsIgnoreCase(NGAY) || arr[0].length() == 0 || arr.length == 1 ? "" : arr[0] + ", ";

		// For case had data
		String[] arr2 = text.trim().split(" ");
		String last = arr2[arr2.length - 1];
		if (last.matches(".*[0-9][.|\\s]*")) {
			return null;
		}
		// For case hasn't fill data yet
		this.isFillIssuedDate = true;
		return address + "Ngày " + data.getDateIssuedStr(3) + " tháng " + data.getDateIssuedStr(2) + " năm "
				+ data.getDateIssuedStr(1);
	}

	private void changeText(XWPFParagraph p, String newText) {
		List<XWPFRun> runs = p.getRuns();
		for (int i = runs.size() - 1; i > 0; i--) {
			p.removeRun(i);
		}
		XWPFRun run = runs.get(0);
		run.setText(newText, 0);
	}
}
