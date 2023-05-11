package com.vz.backend.business.dto;

import lombok.Getter;
import lombok.Setter;
import org.apache.commons.io.IOUtils;
import org.springframework.core.io.Resource;
import org.springframework.web.multipart.MultipartFile;

import java.io.*;
import java.nio.file.Path;

public class CustomMultipartFile implements MultipartFile {
    private final String filename;
    private final String originalFilename;
    private final String contentType;
    private final byte[] bytes;


    public CustomMultipartFile(Resource resource, String fileName, String originalFilename, String contentType) {
        this.filename = fileName;
        this.originalFilename = originalFilename;
        InputStream is;
        try {
            is = resource.getInputStream();
            this.bytes = IOUtils.toByteArray(is);
            is.close();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        this.contentType = contentType;
    }

    public CustomMultipartFile(FileInputStream fileInputStream, String fileName, String originalFilename, String contentType) throws IOException {
        this.filename = fileName;
        this.originalFilename = originalFilename;
        try {
            this.bytes = IOUtils.toByteArray(fileInputStream);
        } catch (IOException e) {
            throw new RuntimeException(e);
        } finally {
            fileInputStream.close();
        }
        this.contentType = contentType;
    }

    @Override
    public String getName() {
        return this.filename;
    }

    @Override
    public String getOriginalFilename() {
        return this.originalFilename;
    }

    @Override
    public String getContentType() {
        return this.contentType;
    }

    @Override
    public boolean isEmpty() {
        return bytes.length == 0;
    }

    @Override
    public long getSize() {
        return bytes.length;
    }

    @Override
    public byte[] getBytes() throws IOException {
        return bytes;
    }

    @Override
    public InputStream getInputStream() throws IOException {
        return new ByteArrayInputStream(bytes);
    }

    @Override
    public Resource getResource() {
        return MultipartFile.super.getResource();
    }

    @Override
    public void transferTo(File dest) throws IOException, IllegalStateException {

    }

    @Override
    public void transferTo(Path dest) throws IOException, IllegalStateException {
        MultipartFile.super.transferTo(dest);
    }
}
