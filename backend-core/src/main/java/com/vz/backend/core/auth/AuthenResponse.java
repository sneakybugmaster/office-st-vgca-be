package com.vz.backend.core.auth;

import java.util.List;

import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.Token;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class AuthenResponse {
	private Token tokenInfo;
	private User userInfo;
	private List<Module> moduleList;
}
