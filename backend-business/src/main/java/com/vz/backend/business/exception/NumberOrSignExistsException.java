package com.vz.backend.business.exception;

import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

@ResponseStatus(HttpStatus.CONFLICT)
public class NumberOrSignExistsException extends RuntimeException {

    public NumberOrSignExistsException(final String message) {
        super(message);
    }
}
