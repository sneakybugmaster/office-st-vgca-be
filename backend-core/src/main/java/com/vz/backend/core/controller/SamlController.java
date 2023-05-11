package com.vz.backend.core.controller;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.ModelAndView;

import com.onelogin.saml2.Auth;
import com.onelogin.saml2.exception.Error;
import com.onelogin.saml2.exception.SettingsException;
import com.vz.backend.core.auth.TokenHelper;
import com.vz.backend.core.domain.Token;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.UserService;

@RestController
@RequestMapping("/saml")
public class SamlController {

	@Autowired
	private TokenHelper tokenProvider;

	@Autowired
	private UserService userService;

	@Value("${front-end.cas-login}")
	private String frontendLogin;

	@Value("${hldap.orgId:}")
	private Long ldapOrgId;

	@Value("${hldap.positionId:}")
	private Long ldapPositionId;

	@Value("${hldap.clientId:1}")
	private Long ldapClientId;

	@Value("${hldap.mail:@vgisc.com}")
	private String ldapMail;

	@GetMapping("/login")
	public ResponseEntity<Boolean> authenticationSaml(HttpServletRequest request, HttpServletResponse response) {
		try {
			Auth auth = new Auth(request, response);
			auth.login();
			return new ResponseEntity<>(Boolean.TRUE, HttpStatus.OK);
		} catch (SettingsException | Error | IOException e) {
			e.printStackTrace();
			return new ResponseEntity<>(Boolean.FALSE, HttpStatus.OK);
		}
	}

	@GetMapping("/login2")
	public ResponseEntity<String> login2() {
		return new ResponseEntity<>("abc", HttpStatus.OK);
	}

	@GetMapping("/logout")
	public void logout(HttpServletRequest request, HttpServletResponse response) {
		try {
			Auth auth = new Auth(request, response);
			auth.logout();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@PostMapping("/sls")
	public String sls(HttpServletRequest request, HttpServletResponse response) {
		try {
			Auth auth = new Auth(request, response);
			auth.processSLO();
			List<String> errors = auth.getErrors();
			if (errors == null || errors.isEmpty()) {
				response.sendRedirect("/api/saml/login");
				return null;
			}
			StringBuilder sbError = new StringBuilder();
			errors.forEach(error -> {
				sbError.append(error).append('\n');
			});
			return sbError.toString();
		} catch (Exception e) {
			e.printStackTrace();
		}
		return "logout error";
	}

	@PostMapping("/wso2-back")
	public ModelAndView samlPost(HttpServletRequest request, HttpServletResponse response) {
		try {
			Auth auth = new Auth(request, response);
			auth.processResponse();

			if (!auth.isAuthenticated()) {
				return new ModelAndView("redirect:login2");
			}

			//			System.out.println(request.getQueryString());
			if (auth.getNameId() == null) {
				throw new RestExceptionHandler("Không tồn tại nameId");
			}
			String userName = convertNameId2Username(auth.getNameId(), ldapMail);

			User u = userService.findByUserNameAndClientId(userName, ldapClientId);

			if (u == null) {
				User user = new User();
				user.setClientId(ldapClientId);
				user.setActive(true);
				user.setUserName(userName);
				user.setFullName(userName);
				user.setEmail(userName + ldapMail);
				user.setLdap(true);
				user.setOrg(ldapOrgId);
				user.setPosition(ldapPositionId);
				userService.add(user);
			}

			//			return new ModelAndView("redirect:login2?user=" + userName);
			Token tokenInfo = tokenProvider.generateToken(userName);
			return new ModelAndView("redirect:" + frontendLogin + tokenInfo.getAccessToken());
		} catch (Exception e) {
			e.printStackTrace();
			return new ModelAndView("redirect:login2?e="+e.getMessage());
		}
	}

	public static String convertNameId2Username(String nameId, String mail) {
		nameId = nameId.replace(mail, "");
		return nameId;
	}

	//	@PostMapping("/wso2-back")
	//	public ResponseEntity<Boolean> samlPost(
	//			@RequestParam(value = "SAMLResponse", required = false) String week) {
	//		System.out.println(week);
	//		return new ResponseEntity<>(Boolean.TRUE, HttpStatus.OK);
	//	}
}
