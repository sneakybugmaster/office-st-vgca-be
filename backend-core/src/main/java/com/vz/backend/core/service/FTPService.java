package com.vz.backend.core.service;

import com.vz.backend.util.DateTimeUtils;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPClient;
import org.apache.commons.net.ftp.FTPReply;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Date;

@Slf4j
@Service
public class FTPService {
    @Value("${ftpConfigs.host}")
    private String FTP_SERVER_ADDRESS;

    @Value("${ftpConfigs.port}")
    private int FTP_SERVER_PORT_NUMBER;

    @Value("${ftpConfigs.timeout: 0}")
    private int FTP_TIMEOUT;

    @Value("${ftpConfigs.username: ''}")
    private String FTP_USERNAME;

    @Value("${ftpConfigs.password: ''}")
    private String FTP_PASSWORD;

    private static final String BASE_DIR = "office";
    public static final String SLASH = "/";
    private static final String BASE_PATH = SLASH + BASE_DIR;
    private static final String UPLOADS = "uploads";
    public static final String TRASH_DIR = "trash";
    private final Path trash = Paths.get(getTrashPath(new Date()));
    private FTPClient ftpClient;

    public static String getTrashPath(Date date) {
        return TRASH_DIR + SLASH + TRASH_DIR + "__"
                + DateTimeUtils.convertDateToStringPattern(date, DateTimeUtils.dateStringFormat);
    }

    public FTPService() {
        if (!Files.exists(trash)) {
            this.init();
        }
    }

    public void init() {
        try {
            Files.createDirectories(trash);
        } catch (IOException e) {
            throw new RuntimeException("Could not initialize folder for upload!");
        }
    }

    /**
     * Delete file from FTP Server
     *
     * @param fileNameRemote
     * @param dirRemote
     */
    public boolean delete(String fileNameRemote, String dirRemote) {
        if (dirRemote == null) {
            dirRemote = UPLOADS;
        }

        log.info("File " + fileNameRemote + " is deleting...");
        try {
            connect();
            return deleteBasic(fileNameRemote, dirRemote);
        } catch (Exception ex) {
            log.error("Error: " + ex.getMessage());
            ex.printStackTrace();
            return false;
        } finally {
            disconnect();
        }
    }

    @SuppressWarnings("resource")
    public Resource load(String fileNameRemote, String dirRemote) {
        if (dirRemote == null) {
            dirRemote = UPLOADS;
        }
        Path tmpPath = trash.resolve(fileNameRemote);
        try {
            File tmpFile = tmpPath.toFile();
            try (OutputStream outputStream = new FileOutputStream(tmpFile)) {
                if (this.download(fileNameRemote, dirRemote, outputStream)) {
                    return new UrlResource("file:///" + tmpFile.getCanonicalPath());
                }
            }
        } catch (FileNotFoundException e1) {
            e1.printStackTrace();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Download file from FTP Server
     *
     * @param fileNameRemote
     * @param dirRemote
     * @param outputStream
     */
    public boolean download(String fileNameRemote, String dirRemote, OutputStream outputStream) {
        if (dirRemote == null) {
            dirRemote = UPLOADS;
        }

        try {
            connect();
            log.info("File " + fileNameRemote + " is downloading...");
            String ftpFilePath = BASE_PATH + SLASH + dirRemote + SLASH + fileNameRemote;
            return downloadBasic(ftpFilePath, outputStream);
        } catch (Exception ex) {
            log.error("Error: " + ex.getMessage());
            ex.printStackTrace();
            return false;
        } finally {
            disconnect();
        }
    }

    private boolean uploadBasic(String fileNameToRemote, InputStream inputStream, String dirRemote) throws Exception {
        ftpClient.setFileType(FTP.BINARY_FILE_TYPE);
        ftpClient.setFileTransferMode(FTPClient.BINARY_FILE_TYPE);
        log.info("Start uploading file");
        this.switch2Dir(SLASH + dirRemote);
        if (checkFileExists(fileNameToRemote, dirRemote)) {
            log.info("File " + fileNameToRemote + " is exist...");
            log.error("The file is uploaded failed.");
            return false;
        }
        String ftpFilePath = ftpClient.printWorkingDirectory() + SLASH + fileNameToRemote;
        boolean result = ftpClient.storeFile(ftpFilePath, inputStream);
        if (result) {
            log.info("The file is uploaded successfully.");
        } else {
            log.error("The file is uploaded failed.");
        }
        return result;
    }

    private boolean deleteBasic(String fileNameRemote, String dirRemote) throws IOException {
        String ftpFilePath = BASE_PATH + SLASH + dirRemote + SLASH + fileNameRemote;
        String currentPath = ftpClient.printWorkingDirectory();
        if (!checkFileExists(fileNameRemote, dirRemote)) {
            log.info("File " + fileNameRemote + " is not exist...");
            log.error("The file is delete failed.");
            return false;
        }

        if (currentPath != null && currentPath.indexOf("delete") > -1) {
            currentPath = currentPath.substring(0, currentPath.indexOf("/office"));
        }
        boolean result = ftpClient.deleteFile(currentPath + ftpFilePath);
        if (result) {
            log.info("The file is delete successfully.");
        } else {
            log.error("The file is delete failed.");
        }
        return result;
    }

    private boolean downloadBasic(String ftpFilePath, OutputStream outputStream) throws IOException {
        ftpClient.setFileType(FTPClient.BINARY_FILE_TYPE);
        boolean result = ftpClient.retrieveFile(ftpClient.printWorkingDirectory() + ftpFilePath, outputStream);
        if (result) {
            log.info("The file is download successfully.");
        } else {
            log.error("The file is download failed.");
        }
        return result;
    }

    public boolean move(String fileNameRemote, String frDirRemote, String toDirRemote) {
        Path tmpPath = trash.resolve(fileNameRemote);
        try {
            connect();
            // download file
            String ftpFilePath = BASE_PATH + SLASH + frDirRemote + SLASH + fileNameRemote;
            File tmpFile = tmpPath.toFile();
            boolean result;
            try (OutputStream outputStream = new FileOutputStream(tmpFile)) {
                result = downloadBasic(ftpFilePath, outputStream);
            }
            if (!result) {
                log.error("Cannot move file " + fileNameRemote);
                return false;
            }

            // upload file
            try (InputStream inputStream = new FileInputStream(tmpFile)) {
                result = uploadBasic(fileNameRemote, inputStream, toDirRemote);
            }
            if (!result) {
                log.error("Cannot move file " + fileNameRemote);
                return false;
            }

            // delete fileNameRemote
            result = deleteBasic(fileNameRemote, frDirRemote);
            if (result) {
                log.info("The file is moved successfully.");
            } else {
                log.info("The file is movedd failed.");
            }
            return result;
        } catch (Exception ex) {
            log.error("Error: " + ex.getMessage());
            ex.printStackTrace();
            return false;
        } finally {
            if (tmpPath != null) {
                try {
                    Files.delete(tmpPath);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            disconnect();
        }
    }

    /**
     * Copy file to FTP Server
     *
     * @param fileNameFrRemote
     * @param fileNameToRemote
     * @return
     */
    public boolean copy(String fileNameFrRemote, String fileNameToRemote) {
        String dirRemote = UPLOADS;
        Path tmpPath = trash.resolve(fileNameFrRemote);
        try {
            connect();
            // download file
            String ftpFilePath = BASE_PATH + SLASH + dirRemote + SLASH + fileNameFrRemote;
            File tmpFile = tmpPath.toFile();
            boolean result;
            try (OutputStream outputStream = new FileOutputStream(tmpFile)) {
                result = downloadBasic(ftpFilePath, outputStream);
            }
            if (!result) {
                log.error("Cannot copy file " + fileNameFrRemote);
                return false;
            }

            // upload file
            try (InputStream inputStream = new FileInputStream(tmpFile)) {
                result = uploadBasic(fileNameToRemote, inputStream, dirRemote);
            }
            if (result) {
                log.info("The file is copied successfully.");
            } else {
                log.info("The file is copied failed.");
            }
            return result;
        } catch (Exception ex) {
            log.error("Error: " + ex.getMessage());
            ex.printStackTrace();
            return false;
        } finally {
            if (tmpPath != null) {
                try {
                    Files.delete(tmpPath);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
            disconnect();
        }
    }

    /**
     * Upload file from FTP Server
     *
     * @param fileNameRemote
     * @param dirRemote
     * @param inputStream
     * @return
     */
    public boolean upload(String fileNameRemote, String dirRemote, InputStream inputStream) {
        if (dirRemote == null) {
            dirRemote = UPLOADS;
        }
        try {
            connect();
            return uploadBasic(fileNameRemote, inputStream, dirRemote);
        } catch (Exception ex) {
            log.error("Error: " + ex.getMessage());
            ex.printStackTrace();
            return false;
        } finally {
            disconnect();
        }
    }

    /**
     * Determines whether a file exists or not
     *
     * @return true if exists, false otherwise
     * @throws IOException thrown if any I/O error occurred.
     */
    boolean checkFileExists(String fileNameRemote, String dirRemote) throws IOException {
        String ftpFilePath = ftpClient.printWorkingDirectory() + SLASH + fileNameRemote;
        try (InputStream inputStream = ftpClient.retrieveFileStream(ftpFilePath)) {
            int returnCode = ftpClient.getReplyCode();
            if (inputStream == null || returnCode == 550) {
                return false;
            }
            return true;
        }
    }

    /**
     * Switch to the upload directory remote
     *
     * @param dirPath
     * @return
     * @throws Exception
     */
    private boolean switch2Dir(String dirPath) throws Exception {
        connect();

        String pathRoot = ftpClient.printWorkingDirectory() + BASE_PATH;
        // If the directory does not have a creation directory
        if (!ftpClient.changeWorkingDirectory(pathRoot)) {
            ftpClient.makeDirectory(pathRoot);
        }

        if (!ftpClient.changeWorkingDirectory(pathRoot + dirPath)) {
            String[] dirs = dirPath.split(SLASH);
            String tempPath = pathRoot;
            for (String dir : dirs) {
                if (null == dir || "".equals(dir))
                    continue;
                tempPath += SLASH + dir;
                if (!ftpClient.changeWorkingDirectory(tempPath)) {
                    if (!ftpClient.makeDirectory(tempPath)) {
                        log.error("Cannot make directory contains file");
                        return false;
                    } else {
                        ftpClient.changeWorkingDirectory(tempPath);
                    }
                }
            }
        }
        return true;
    }

    /**
     * Connect FTP Server
     */
    public void connect() {
        if (ftpClient != null && ftpClient.isConnected()) {
            return;
        }
        ftpClient = new FTPClient();
        try {
            log.info("Connecting ftp server...");
            ftpClient.setControlEncoding("UTF-8");
            ftpClient.setDefaultTimeout(FTP_TIMEOUT);
            // connect to ftp server
            ftpClient.connect(FTP_SERVER_ADDRESS, FTP_SERVER_PORT_NUMBER);
            // run the passive mode command
            ftpClient.enterLocalPassiveMode();
            // check reply code
            // If data cannot read , may be cause this line has been called more 1 times
            if (!FTPReply.isPositiveCompletion(ftpClient.getReplyCode())) {
                disconnect();
                throw new IOException("FTP server not respond!");
            } else {
                ftpClient.setSoTimeout(FTP_TIMEOUT);
                // login ftp server
                if (!ftpClient.login(FTP_USERNAME, FTP_PASSWORD)) {
                    throw new IOException("Username or password is incorrect!");
                }
                ftpClient.setDataTimeout(FTP_TIMEOUT);
                log.info("Connected");
            }

        } catch (IOException ex) {
            ex.printStackTrace();
        }
    }

    /**
     * Disconnect FTP Server
     */
    public void disconnect() {
        if (ftpClient != null && ftpClient.isConnected()) {
            try {
                ftpClient.logout();
                ftpClient.disconnect();
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }
}
