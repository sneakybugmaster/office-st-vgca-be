package com.vz.backend.core.util;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
@Slf4j
public final class StreamUtils {
    public static void closeOutputStream(OutputStream os) {
        if (os != null) {
            try {
                os.close();
            } catch (IOException e) {
                log.error(e.getMessage(), e);
            }
        }
    }
}
