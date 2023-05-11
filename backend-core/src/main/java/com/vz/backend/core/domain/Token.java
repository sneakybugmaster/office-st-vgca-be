package com.vz.backend.core.domain;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author DucND
 * @date Apr 15, 2020
 */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Token {
	private String accessToken;
	private Date timeExprise;
}
